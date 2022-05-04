/**
 * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
 *
 * This file is part of ScalaFix. ScalaFix is free software: you can
 * redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * ScalaFix is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of a MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * ScalaFix. If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.scalafix.finite

import it.unich.scalafix.*
import it.unich.scalafix.graphs.*
import it.unich.scalafix.assignments.*
import it.unich.scalafix.lattice.Domain

/**
 * This solver is a commodity interface for the other finite fixpoint solvers.
 * It takes some parameters as inputs and plans a sequence of actions in order
 * to obtain the desired solutions as the output.
 */
object FiniteFixpointSolver:

  import FixpointSolver.*

  /**
   * Returns parameters for solving an equation system with the standard CC77
   * approach
   *
   * @param solver
   *   the real solver to use
   * @param wideningComboAssn
   *   a combo used for widenings
   * @param narrowingComboAssn
   *   a combo used for narrowings
   */
  def CC77[U, V](
      solver: Solver.Solver,
      start: Assignment[U, V],
      wideningComboAssn: ComboAssignment[U, V],
      narrowingComboAssn: ComboAssignment[U, V]
  ): Params[U, V] =
    Params[U, V](
      solver,
      start,
      ComboLocation.Loop,
      ComboScope.Standard,
      ComboStrategy.TwoPhases,
      RestartStrategy.None,
      wideningComboAssn,
      narrowingComboAssn,
      FixpointSolverTracer.empty[U, V]
    )

  /**
   * Solves the equation system using the parameters specified in p.
   *
   * @param eqs
   *   the equation system to solve
   * @param params
   *   the parameters for the solver
   */
  def apply[U, V: Domain, E, EQS <: GraphEquationSystem[U, V, E, EQS]](
      eqs: EQS,
      params: Params[U, V]
  ): MutableAssignment[U, V] =
    import params.*

    val startAssn = params.start

    val ordering1: Option[GraphOrdering[U]] = (solver, comboscope) match
      case (Solver.HierarchicalOrderingSolver, _) =>
        Some(HierarchicalOrdering(DFOrdering(eqs)))
      case (Solver.PriorityWorkListSolver, _) | (_, ComboScope.Localized) =>
        Some(DFOrdering(eqs))
      case _ =>
        None

    val ordering: Option[GraphOrdering[U]] = (combolocation: @unchecked) match
      case ComboLocation.None | ComboLocation.All =>
        None
      case ComboLocation.Loop =>
        ordering1 orElse Some(DFOrdering(eqs))

    val restart: (V, V) => Boolean =
      if restartstrategy == RestartStrategy.Restart then { (x, y) => summon[Domain[V]].lt(x, y) }
      else { (_, _) => false }

    (combostrategy: @unchecked) match
      case ComboStrategy.OnlyWidening =>
        val widening = comboFilter[U, V](wideningComboAssn, combolocation, ordering)
        val withWidening = comboApply(eqs, widening, comboscope, ordering)
        applySolver(solver, withWidening, startAssn, ordering, restart, tracer)
      case ComboStrategy.TwoPhases =>
        val widening = comboFilter[U, V](wideningComboAssn, combolocation, ordering)
        val withWidening = comboApply(eqs, widening, comboscope, ordering)
        tracer.ascendingBegins(startAssn)
        val ascendingAssignment =
          applySolver(solver, withWidening, startAssn, ordering, restart, tracer)
        val narrowing = comboFilter[U, V](narrowingComboAssn, combolocation, ordering)
        // localizing narrowings does not seem useful
        val withNarrowing = comboApply(eqs, narrowing, ComboScope.Standard, ordering)
        tracer.descendingBegins(ascendingAssignment)
        applySolver(solver, withNarrowing, ascendingAssignment, ordering, restart, tracer)
      case ComboStrategy.Warrowing =>
        if comboscope == ComboScope.Localized then
          val widening = comboFilter[U, V](wideningComboAssn, combolocation, ordering)
          val narrowing = comboFilter[U, V](narrowingComboAssn, combolocation, ordering)
          val withUpdate =
            if widening.isEmpty && narrowing.isEmpty then eqs
            else eqs.withLocalizedWarrowing(widening, narrowing, ordering.get)
          applySolver(solver, withUpdate, startAssn, ordering, restart, tracer)
        else
          val warrowingAssignment = comboFilter[U, V](
            ComboAssignment.warrowing(wideningComboAssn, narrowingComboAssn),
            combolocation,
            ordering
          )
          val eqsWithWarrowing = comboApply(eqs, warrowingAssignment, comboscope, ordering)
          applySolver(solver, eqsWithWarrowing, startAssn, ordering, restart, tracer)

  /**
   * Given an equation system and a combo assignment, filter the assignment
   * according to what specified in the parameter location and the graph
   * ordering.
   *
   * @param eqs
   *   an equation system
   * @param comboAssn
   *   a combo assignment
   * @param location
   *   input parameter which drives the filtering by specifying where to put
   *   combos
   * @param ordering
   *   a GraphOrdering used when we need to detect heads
   */
  private def comboFilter[U, V](
      comboAssn: ComboAssignment[U, V],
      location: ComboLocation.Value,
      ordering: Option[GraphOrdering[U]]
  ): ComboAssignment[U, V] =
    (location: @unchecked) match
      case ComboLocation.None => ComboAssignment.empty
      case ComboLocation.All  => comboAssn
      case ComboLocation.Loop => comboAssn.restrict(ordering.get.isHead)

  /**
   * Apply a given combo assignment to an equation system, generating a new
   * equation system.
   *
   * @param eqs
   *   the equation system
   * @param combos
   *   a combo assignment
   * @param scope
   *   an input parameters which determines how we want to apply combos (such as
   *   localized or standard)
   * @param ordering
   *   an optional ordering on unknowns to be used for localized combos.
   */
  private def comboApply[U, V, E, EQS <: FiniteEquationSystem[U, V, EQS]](
      eqs: EQS,
      combos: ComboAssignment[U, V],
      scope: ComboScope.Value,
      ordering: Option[Ordering[U]]
  ): EQS =
    (scope: @unchecked) match
      case ComboScope.Standard => eqs.withCombos(combos)
      case ComboScope.Localized =>
        eqs match
          // todoss
          case eqs: GraphEquationSystem[U, V, E, EQS] @unchecked =>
            eqs.withLocalizedCombos(combos, ordering.get)
          case _ => throw new DriverBadParameters("Localized combos needs a GraphEquationSystem")

  /**
   * Apply a fixpoint solver given standard parameters
   *
   * @param solver
   *   the solver to apply
   * @param eqs
   *   the equation system
   * @param start
   *   the initial assignment
   * @param ordering
   *   an optional ordering on unknowns
   * @param restart
   *   a restart strategy
   * @param tracer
   *   a fixpoint solver tracer
   * @return
   *   an assignment with the solution of the equation system
   */
  private def applySolver[U, V, EQS <: FiniteEquationSystem[U, V, EQS]](
      solver: Solver.Solver,
      eqs: EQS,
      start: Assignment[U, V],
      ordering: Option[Ordering[U]],
      restart: (V, V) => Boolean,
      tracer: FixpointSolverTracer[U, V]
  ): MutableAssignment[U, V] =
    (solver: @unchecked) match
      case Solver.RoundRobinSolver => RoundRobinSolver(eqs)(start, tracer)
      case Solver.KleeneSolver     => KleeneSolver(eqs)(start, tracer)
      case Solver.WorkListSolver   => WorkListSolver(eqs)(start, tracer)
      case Solver.PriorityWorkListSolver =>
        PriorityWorkListSolver(eqs)(start, ordering.get, restart, tracer)
      case Solver.HierarchicalOrderingSolver =>
        ordering.get match
          case ho: HierarchicalOrdering[U] => HierarchicalOrderingSolver(eqs)(start, ho, tracer)
          case _ =>
            throw new DriverBadParameters(
              "Ordering must be hierarchical for the HierarchicalOrderingSolver to work"
            )

  /**
   * Parameters for this driver
   *
   * @param solver
   *   the real solver to use
   * @param start
   *   an optional initial assignment
   * @param combolocation
   *   where to put combos
   * @param comboscope
   *   how to apply combos (standard, localized, etc...)
   * @param combostrategy
   *   single phase, two phase, warrowing
   * @param restartstrategy
   *   restart strategy to apply in supported solvers
   * @param wideningComboAssn
   *   a combo used for widenings
   * @param narrowingComboAssn
   *   a combo used for narrowings
   * @param tracer
   *   a fixpoint solver tracer
   */
  case class Params[U, V](
      solver: Solver.Solver,
      start: Assignment[U, V],
      combolocation: ComboLocation.ComboLocation,
      comboscope: ComboScope.ComboScope,
      combostrategy: ComboStrategy.ComboStrategy,
      restartstrategy: RestartStrategy.RestartStrategy,
      wideningComboAssn: ComboAssignment[U, V],
      narrowingComboAssn: ComboAssignment[U, V],
      tracer: FixpointSolverTracer[U, V]
  )
