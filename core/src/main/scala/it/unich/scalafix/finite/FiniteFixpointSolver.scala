/**
  * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.finite

import it.unich.scalafix._
import it.unich.scalafix.lattice.Domain

/**
  * This solver is a commodity interface for the other finite fixpoint solvers. It takes some parameters
  * as inputs and plans a sequence of actions in order to obtain the desired solutions as the output.
  */
object FiniteFixpointSolver {

  import FixpointSolver._

  /**
    * Returns parameters for solving an equation system with the standard CC77 approach
    *
    * @param solver           the real solver to use
    * @param wideningBoxAssn  a box used for widenings
    * @param narrowingBoxAssn a box used for narrowings
    */
  def CC77[U, V](
                  solver: Solver.Solver,
                  wideningBoxAssn: BoxAssignment[U, V],
                  narrowingBoxAssn: BoxAssignment[U, V]
                ): Params[U, V] =
    Params[U, V](solver, None, BoxLocation.Loop, BoxScope.Standard, BoxStrategy.TwoPhases, RestartStrategy.None,
      wideningBoxAssn, narrowingBoxAssn, FixpointSolverTracer.empty[U,V])

  /**
    * Solves the equation system using the parameters specified in p.
    *
    * @param eqs    the equation system to solve
    * @param params the parameters for the solver
    */
  def apply[U, V: Domain, E](eqs: GraphEquationSystem[U, V, E], params: Params[U, V]): Assignment[U, V] = {
    import params._

    val startAssn = params.start.getOrElse(eqs.initial)

    val ordering1: Option[GraphOrdering[U]] = (solver, boxscope) match {
      case (Solver.HierarchicalOrderingSolver, _) =>
        Some(HierarchicalOrdering(DFOrdering(eqs)))
      case (Solver.PriorityWorkListSolver, _) | (_, BoxScope.Localized) =>
        Some(DFOrdering(eqs))
      case _ =>
        None
    }

    val ordering: Option[GraphOrdering[U]] = boxlocation match {
      case BoxLocation.None | BoxLocation.All =>
        None
      case BoxLocation.Loop =>
        ordering1 orElse Some(DFOrdering(eqs))
    }

    val restart: (V, V) => Boolean =
      if (restartstrategy == RestartStrategy.Restart) { (x, y) => Domain[V].lt(x, y) }
      else { (_, _) => false }

    boxstrategy match {
      case BoxStrategy.OnlyWidening =>
        val widening = boxFilter[U, V](eqs, wideningBoxAssn, boxlocation, ordering)
        val withWidening = boxApply(eqs, widening, boxscope, ordering)
        applySolver(solver, withWidening, startAssn, ordering, restart, tracer)
      case BoxStrategy.TwoPhases =>
        val widening = boxFilter[U, V](eqs, wideningBoxAssn, boxlocation, ordering)
        val withWidening = boxApply(eqs, widening, boxscope, ordering)
        tracer.ascendingBegins(startAssn)
        val ascendingAssignment = applySolver(solver, withWidening, startAssn, ordering, restart, tracer)
        val narrowing = boxFilter[U, V](eqs, narrowingBoxAssn, boxlocation, ordering)
        // localizing narrowings does not seem useful
        val withNarrowing = boxApply(eqs, narrowing, BoxScope.Standard, ordering)
        tracer.descendingBegins(startAssn)
        applySolver(solver, withNarrowing, ascendingAssignment, ordering, restart, tracer)
      case BoxStrategy.Warrowing =>
        if (boxscope == BoxScope.Localized) {
          val widening = boxFilter[U, V](eqs, wideningBoxAssn, boxlocation, ordering)
          val narrowing = boxFilter[U, V](eqs, narrowingBoxAssn, boxlocation, ordering)
          val withUpdate = if (widening.isEmpty && narrowing.isEmpty)
            eqs
          else
            eqs.withLocalizedWarrowing(widening, narrowing, ordering.get)
          applySolver(solver, withUpdate, startAssn, ordering, restart, tracer)
        } else {
          val warrowingAssignment = boxFilter[U, V](eqs, BoxAssignment.warrowing(wideningBoxAssn, narrowingBoxAssn), boxlocation, ordering)
          val eqsWithWarrowing = boxApply(eqs, warrowingAssignment, boxscope, ordering)
          applySolver(solver, eqsWithWarrowing, startAssn, ordering, restart, tracer)
        }
    }
  }

  /**
    * Given an equation system and a box assignment, filter the assignment according to what specified in the
    * parameter location and the graph ordering.
    *
    * @param eqs      an equation system
    * @param boxAssn  a box assignment
    * @param location input parameter which drives the filtering by specifying where to put boxes
    * @param ordering a GraphOrdering used when we need to detect heads
    */
  private def boxFilter[U, V](
                               eqs: FiniteEquationSystem[U, V],
                               boxAssn: BoxAssignment[U, V],
                               location: BoxLocation.Value,
                               ordering: Option[GraphOrdering[U]]
                             ): BoxAssignment[U, V] =
    location match {
      case BoxLocation.None => BoxAssignment.empty
      case BoxLocation.All => boxAssn
      case BoxLocation.Loop => boxAssn.restrict(ordering.get.isHead)
    }

  /**
    * Apply a given box assignment to an equation system, generating a new equation system.
    *
    * @param eqs      the equation system
    * @param boxes    a box assignment
    * @param scope    an input parameters which determines how we want to apply boxes (such as localized or standard)
    * @param ordering an optional ordering on unknowns to be used for localized boxes.
    */
  private def boxApply[U, V, E](
                                 eqs: FiniteEquationSystem[U, V],
                                 boxes: BoxAssignment[U, V],
                                 scope: BoxScope.Value,
                                 ordering: Option[Ordering[U]]
                               ): FiniteEquationSystem[U, V] = {
    scope match {
      case BoxScope.Standard => eqs.withBoxes(boxes)
      case BoxScope.Localized => eqs match {
        case eqs: GraphEquationSystem[U, V, _] => eqs.withLocalizedBoxes(boxes, ordering.get)
        case _ => throw new DriverBadParameters("Localized boxes needs a GraphEquationSystem")
      }
    }
  }

  /**
    * Apply a fixpoint solver given standard parameters
    *
    * @param solver   the solver to apply
    * @param eqs      the equation system
    * @param start    the initial assignment
    * @param ordering an optional ordering on unknowns
    * @param restart  a restart strategy
    * @param tracer a fixpoint solver tracer
    * @return an assignment with the solution of the equation system
    */
  private def applySolver[U, V](
                                 solver: Solver.Solver,
                                 eqs: FiniteEquationSystem[U, V],
                                 start: Assignment[U, V],
                                 ordering: Option[Ordering[U]],
                                 restart: (V, V) => Boolean,
                                 tracer: FixpointSolverTracer[U, V]
                               ): Assignment[U, V] = {
    solver match {
      case Solver.RoundRobinSolver => RoundRobinSolver(eqs)(start, tracer)
      case Solver.KleeneSolver => KleeneSolver(eqs)(start, tracer)
      case Solver.WorkListSolver => WorkListSolver(eqs)(start, tracer)
      case Solver.PriorityWorkListSolver => PriorityWorkListSolver(eqs)(start, ordering.get, restart, tracer)
      case Solver.HierarchicalOrderingSolver =>
        ordering.get match {
          case ho: HierarchicalOrdering[U] => HierarchicalOrderingSolver(eqs)(start, ho, tracer)
          case _ => throw new DriverBadParameters("Ordering must be hierarchical for the HierarchicalOrderingSolver to work")
        }
    }
  }

  /**
    * Parameters for this driver
    *
    * @param solver           the real solver to use
    * @param start            an optional initial assignment
    * @param boxlocation      where to put boxes
    * @param boxscope         how to apply boxes (standard, localized, etc...)
    * @param boxstrategy      single phase, two phase, warrowing
    * @param restartstrategy  restart strategy to apply in supported solvers
    * @param wideningBoxAssn  a box used for widenings
    * @param narrowingBoxAssn a box used for narrowings
    * @param tracer         a fixpoint solver tracer
    */
  case class Params[U, V](
                           solver: Solver.Solver,
                           start: Option[Assignment[U, V]],
                           boxlocation: BoxLocation.BoxLocation,
                           boxscope: BoxScope.BoxScope,
                           boxstrategy: BoxStrategy.BoxStrategy,
                           restartstrategy: RestartStrategy.RestartStrategy,
                           wideningBoxAssn: BoxAssignment[U, V],
                           narrowingBoxAssn: BoxAssignment[U, V],
                           tracer: FixpointSolverTracer[U, V]
                         )

}
