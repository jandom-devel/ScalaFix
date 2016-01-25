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
  * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.drivers

import it.unich.scalafix.FixpointSolverListener.EmptyListener
import it.unich.scalafix._
import it.unich.scalafix.drivers.Driver._
import it.unich.scalafix.finite._
import it.unich.scalafix.lattice._

/**
  * This driver is a commodity interface for solving finite equation systems.
  */
object FiniteDriver extends FixpointSolver with Driver {
  /**
    * @inheritdoc
    * This solver only works with finite equation systems.
    */
  type EQS[U, V] = FiniteEquationSystem[U, V]

  /**
    * Given an equation system and a box assignment, filter the assignment according to what specified in the input
    * parameter location and the graph ordering.
    *
    * @param eqs      an equation system
    * @param box      a box
    * @param location input parameter which drives the filtering by specifying where to put boxes
    * @param ordering a GraphOrdering used when we need to detect heads
    */
  private def boxFilter[U, V](eqs: FiniteEquationSystem[U, V], box: Box[V], location: BoxLocation.Value, ordering: Option[GraphOrdering[U]]): BoxAssignment[U, V] =
    location match {
      case BoxLocation.None => BoxAssignment.empty
      case BoxLocation.All => BoxAssignment(box)
      case BoxLocation.Loop => BoxAssignment(box).restrict(ordering.get.isHead)
    }

  /**
    * Apply a given box assignment to an equation system, generating a new equation system.
    *
    * @param eqs      the equation system
    * @param boxes    a box assignment
    * @param scope    an input parameters which determines how we want to apply boxes (such as localized or standard)
    * @param ordering an optional ordering on unknowns to be used for localized boxes.
    */
  private def boxApply[U, V, E](eqs: FiniteEquationSystem[U, V], boxes: BoxAssignment[U, V], scope: BoxScope.Value,
                                ordering: Option[Ordering[U]]): FiniteEquationSystem[U, V] = {
    scope match {
      case BoxScope.Standard => eqs.withBoxes(boxes)
      case BoxScope.Localized => eqs match {
        case eqs: GraphEquationSystem[U, V, _] => eqs.withLocalizedBoxes(boxes, ordering.get)
        case _ => throw new DriverBadParameters("Localized boxes needs a GraphEquationSystem")
      }
    }
  }

  def addLocalizedBoxes[U, V: DirectedPartialOrdering, E](eqs: GraphEquationSystem[U, V, E], widening: BoxAssignment[U, V],
                                                          narrowing: BoxAssignment[U, V], ordering: Ordering[U]): FiniteEquationSystem[U, V] = {
    val dop = DirectedPartialOrdering[V]
    val newbody = new Body[U, V] {
      def apply(rho: Assignment[U, V]) = {
        (x: U) =>
          val contributions = for (e <- eqs.ingoing(x)) yield {
            val contrib = eqs.edgeAction(rho)(e)
            val boxapply = eqs.sources(e).exists(ordering.lteq(x, _)) && !dop.lteq(contrib, rho(x))
            (contrib, boxapply)
          }
          // if contribution is empty the unknown x has no right hand side... it seems
          // reasonable to return the old value.
          if (contributions.isEmpty)
            rho(x)
          else {
            val result = contributions reduce { (x: (V, Boolean), y: (V, Boolean)) => (dop.upperBound(x._1, y._1), x._2 || y._2) }
            //println((x, rho(x), contributions))
            if (result._2) {
              widening(x)(rho(x), result._1)
            } else if (dop.lt(result._1, rho(x))) narrowing(x)(rho(x), result._1) else result._1
          }
      }
    }

    FiniteEquationSystem(
      body = newbody,
      initial = eqs.initial,
      inputUnknowns = eqs.inputUnknowns,
      unknowns = eqs.unknowns,
      infl = if (widening.areIdempotent && narrowing.areIdempotent) eqs.infl else eqs.infl.withDiagonal
    )
  }

  /**
    * Apply a finite fixpoint solver given standard paramete
    *
    * @param solver   the solver to apply
    * @param eqs      the equation system
    * @param start    the initial assignment
    * @param ordering an optional ordering on unknowns
    * @param restart  a restart strategy
    * @param listener a fixpoint listener
    * @return an assignment with the solution of the equation system
    */
  def applySolver[U, V](
                         solver: Solver.Solver,
                         eqs: FiniteEquationSystem[U, V],
                         start: Assignment[U, V],
                         ordering: Option[Ordering[U]],
                         restart: (V, V) => Boolean,
                         listener: FixpointSolverListener[U, V]
                       ): Assignment[U, V] = {
    solver match {
      case Solver.RoundRobinSolver => RoundRobinSolver(eqs, start, listener)
      case Solver.KleeneSolver => KleeneSolver(eqs, start, listener)
      case Solver.WorkListSolver => WorkListSolver(eqs, start, listener)
      case Solver.PriorityWorkListSolver => PriorityWorkListSolver(eqs, start, ordering.get, restart, listener)
      case Solver.HierarchicalOrderingSolver =>
        ordering.get match {
          case ho: HierarchicalOrdering[U] => HierarchicalOrderingSolver(eqs, start, ho, listener)
          case _ => throw new DriverBadParameters("Ordering bust me a hierarchical order for the HierarchicalOrderingSolver")
        }
    }
  }

  /**
    * Parameters for this driver
    *
    * @param solver          the real solver to use
    * @param start           the initial assignment (null if we want to use the original one)
    * @param boxlocation     where to put boxes
    * @param boxscope        how to apply boxes (standard, localized, etc...)
    * @param boxstrategy     single phase, two phase, warrowing
    * @param restartstrategy if true, apply restart strategy in supported solvers
    * @param wideningBox     a box used for widening
    * @param narrowingBox    a box used for narrowing
    * @param listener        a fixpoint listener
    */
  case class Params[U, V](
                           solver: Solver.Solver,
                           start: Assignment[U, V],
                           boxlocation: BoxLocation.BoxLocation,
                           boxscope: BoxScope.BoxScope,
                           boxstrategy: BoxStrategy.BoxStrategy,
                           restartstrategy: Boolean,
                           wideningBox: Box[V],
                           narrowingBox: Box[V],
                           listener: FixpointSolverListener[U, V]
                         ) extends BaseParams[U, V]

  /**
    * Returns parameters for solving an equation system with the standard CC77 approach
    *
    * @param solver       the real solver to use
    * @param wideningBox  a box used for widening
    * @param narrowingBox a box used for narrowing
    */
  def CC77[U, V](solver: Solver.Solver, wideningBox: Box[V], narrowingBox: Box[V]) =
    Params[U, V](solver, null, BoxLocation.Loop, BoxScope.Standard, BoxStrategy.TwoPhases, false, wideningBox, narrowingBox, EmptyListener)

  /**
    * Solves the equation system using the parameters specified in p.
    *
    * @param eqs    the equation system to solve
    * @param params the parameters for the solver
    */
  def apply[U, V: DirectedPartialOrdering, E](eqs: GraphEquationSystem[U, V, E], params: Params[U, V]): Assignment[U, V] = {
    import params._

    val startAssn = if (params.start == null) eqs.initial else start

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
      if (restartstrategy) { (x, y) => DirectedPartialOrdering[V].lt(x, y) }
      else { (x, y) => false }

    boxstrategy match {
      case BoxStrategy.OnlyWidening =>
        val widening = boxFilter[U, V](eqs, wideningBox, boxlocation, ordering)
        val withWidening = boxApply(eqs, widening, boxscope, ordering)
        applySolver(solver, withWidening, startAssn, ordering, restart, listener)
      case BoxStrategy.TwoPhases =>
        val widening = boxFilter[U, V](eqs, wideningBox, boxlocation, ordering)
        val withWidening = boxApply(eqs, widening, boxscope, ordering)
        val ascendingAssignment = applySolver(solver, withWidening, startAssn, ordering, restart, listener)
        val narrowing = boxFilter[U, V](eqs, narrowingBox, boxlocation, ordering)
        // localizing narrowing does not seem useful
        val withNarrowing = boxApply(eqs, narrowing, BoxScope.Standard, ordering)
        applySolver(solver, withNarrowing, ascendingAssignment, ordering, restart, listener)
      case BoxStrategy.Warrowing =>
        if (boxscope == BoxScope.Localized) {
          val widening = boxFilter[U, V](eqs, wideningBox, boxlocation, ordering)
          val narrowing = boxFilter[U, V](eqs, narrowingBox, boxlocation, ordering)
          val withUpdate = if (widening.isEmpty && narrowing.isEmpty)
            eqs
          else
            addLocalizedBoxes(eqs, widening, narrowing, ordering.get)
          applySolver(solver, withUpdate, startAssn, ordering, restart, listener)
        } else {
          val warrowingAssignment = boxFilter[U, V](eqs, Box.warrowing(wideningBox, narrowingBox), boxlocation, ordering)
          val eqsWithWarrowing = boxApply(eqs, warrowingAssignment, boxscope, ordering)
          applySolver(solver, eqsWithWarrowing, startAssn, ordering, restart, listener)
        }
    }
  }
}
