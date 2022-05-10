/**
 * Copyright 2015, 2016, 2022 Gianluca Amato <gianluca.amato@unich.it>
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
import it.unich.scalafix.FixpointSolver.*
import it.unich.scalafix.graphs.*
import it.unich.scalafix.assignments.*
import it.unich.scalafix.lattice.Domain

/**
 * This solver is a commodity interface for the other fixpoint solvers for
 * finite and graph-based equation systems. It takes some parameters as inputs
 * and plans a sequence of actions in order to obtain the desired solutions as
 * the output.
 */
object FiniteFixpointSolver:

  import FixpointSolver.*

  /**
   * Returns parameters for solving an equation system with the standard CC77
   * approach
   *
   * @param solver
   *   the solver to use
   * @param widenins
   *   an assignment of widenings to unknowns
   * @param narrowings
   *   an assignment of narrowings to unknowns
   */
  def CC77[U, V](
      solver: Solver,
      start: Assignment[U, V],
      widenings: ComboAssignment[U, V],
      narrowings: ComboAssignment[U, V]
  ): Params[U, V] =
    Params[U, V](
      solver,
      start,
      ComboLocation.Loop,
      ComboScope.Standard,
      ComboStrategy.TwoPhases,
      RestartStrategy.None,
      widenings,
      narrowings,
      FixpointSolverTracer.empty
    )

  /**
   * Solves the equation system using the parameters specified in `params`.
   *
   * @param eqs
   *   the equation system to solve.
   * @param params
   *   the parameters for the solver.
   */
  def apply[U, V: Domain, E, EQS <: GraphEquationSystem[U, V, E, EQS]](
      eqs: EQS,
      params: Params[U, V]
  ): MutableAssignment[U, V] =
    import params.*

    val startAssn = start

    val ordering1: Option[UnknownOrdering[U]] = (solver, comboScope) match
      case (Solver.HierarchicalOrderingSolver, _) =>
        Some(HierarchicalOrdering(DFOrdering(eqs)))
      case (Solver.PriorityWorkListSolver, _) | (_, ComboScope.Localized) =>
        Some(DFOrdering(eqs))
      case _ =>
        None

    val ordering: Option[UnknownOrdering[U]] = comboLocation match
      case ComboLocation.None | ComboLocation.All =>
        None
      case ComboLocation.Loop =>
        ordering1 orElse Some(DFOrdering(eqs))

    val restart: (V, V) => Boolean =
      if restartStrategy == RestartStrategy.Restart
      then { summon[Domain[V]].lt }
      else { (_, _) => false }

    comboStrategy match
      case ComboStrategy.OnlyWidening =>
        val widening = comboFilter[U, V](widenings, comboLocation, ordering)
        val withWidening = comboApply(eqs, widening, comboScope, ordering)
        applySolver(solver, withWidening, startAssn, ordering, restart, tracer)
      case ComboStrategy.TwoPhases =>
        val widening = comboFilter[U, V](widenings, comboLocation, ordering)
        val withWidening = comboApply(eqs, widening, comboScope, ordering)
        tracer.ascendingBegins(startAssn)
        val ascendingAssignment =
          applySolver(solver, withWidening, startAssn, ordering, restart, tracer)
        val narrowing = comboFilter[U, V](narrowings, comboLocation, ordering)
        // localizing narrowings does not seem useful
        val withNarrowing = comboApply(eqs, narrowing, ComboScope.Standard, ordering)
        tracer.descendingBegins(ascendingAssignment)
        applySolver(solver, withNarrowing, ascendingAssignment, ordering, restart, tracer)
      case ComboStrategy.Warrowing =>
        if comboScope == ComboScope.Localized then
          val widening = comboFilter[U, V](widenings, comboLocation, ordering)
          val narrowing = comboFilter[U, V](narrowings, comboLocation, ordering)
          val withUpdate =
            if widening.isEmpty && narrowing.isEmpty
            then eqs
            else eqs.withLocalizedWarrowing(widening, narrowing, ordering.get)
          applySolver(solver, withUpdate, startAssn, ordering, restart, tracer)
        else
          val warrowingAssignment = comboFilter[U, V](
            ComboAssignment.warrowing(widenings, narrowings),
            comboLocation,
            ordering
          )
          val eqsWithWarrowing = comboApply(eqs, warrowingAssignment, comboScope, ordering)
          applySolver(solver, eqsWithWarrowing, startAssn, ordering, restart, tracer)

  /**
   * Filter a combo assignment according to what specified in the parameters
   * comboLocation and the unknown ordering.
   *
   * @param combos
   *   a combo assignment.
   * @param comboLocation
   *   pecifying where to put combos.
   * @param unknownOrdering
   *   an optional ordering used when we need to detect loop heads.
   */
  private def comboFilter[U, V](
      combos: ComboAssignment[U, V],
      comboLocation: ComboLocation,
      unknownOrdering: Option[UnknownOrdering[U]]
  ): ComboAssignment[U, V] =
    comboLocation match
      case ComboLocation.None => ComboAssignment.empty
      case ComboLocation.All  => combos
      case ComboLocation.Loop => combos.restrict(unknownOrdering.get.isHead)

  /**
   * Apply a given combo assignment to an equation system, generating a new
   * equation system.
   *
   * @param eqs
   *   the equation system
   * @param combos
   *   a combo assignment
   * @param comboScope
   *   an input parameters which determines how we want to apply combos (such as
   *   localized or standard)
   * @param unknownOrdering
   *   an optional ordering on unknowns to be used for localized combos.
   */
  private def comboApply[U, V, E, EQS <: FiniteEquationSystem[U, V, EQS]](
      eqs: EQS,
      combos: ComboAssignment[U, V],
      comboScope: ComboScope,
      unknownOrdering: Option[Ordering[U]]
  ): EQS =
    comboScope match
      case ComboScope.Standard => eqs.withCombos(combos)
      case ComboScope.Localized =>
        eqs match
          // todoss
          case eqs: GraphEquationSystem[?, ?, ?, ?] =>
            eqs.withLocalizedCombos(combos, unknownOrdering.get)
          case _ => throw new DriverBadParameters("Localized combos needs a GraphEquationSystem")

  /**
   * Apply a fixpoint solver given standard parameters
   *
   * @param solver
   *   the solver to apply.
   * @param eqs
   *   the equation system.
   * @param start
   *   the initial assignment.
   * @param ordering
   *   an optional ordering on unknowns.
   * @param restart
   *   a restart strategy.
   * @param tracer
   *   a fixpoint solver tracer.
   * @return
   *   an assignment with the solution of the equation system.
   */
  private def applySolver[U, V, EQS <: FiniteEquationSystem[U, V, EQS]](
      solver: Solver,
      eqs: EQS,
      start: Assignment[U, V],
      ordering: Option[Ordering[U]],
      restart: (V, V) => Boolean,
      tracer: FixpointSolverTracer[U, V]
  ): MutableAssignment[U, V] =
    solver match
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
   * Parameters for this convenience solver.
   *
   * @param solver
   *   the real solver to use.
   * @param start
   *   an initial assignment.
   * @param comboLocation
   *   where to put widenings and narrowings.
   * @param comboScope
   *   how to apply widenings and narrowings (standard, localized, etc...).
   * @param comboStrategy
   *   strategies for applying widening and arrowings.
   * @param restartStrategy
   *   restart strategy to apply in supported solvers.
   * @param widenings
   *   an assignment of widenings to unknowns
   * @param narrowingComboAssn
   *   an assignment of narrowings to unknowns
   * @param tracer
   *   a fixpoint solver tracer.
   */
  case class Params[U, V](
      solver: Solver,
      start: Assignment[U, V],
      comboLocation: ComboLocation,
      comboScope: ComboScope,
      comboStrategy: ComboStrategy,
      restartStrategy: RestartStrategy,
      widenings: ComboAssignment[U, V],
      narrowings: ComboAssignment[U, V],
      tracer: FixpointSolverTracer[U, V]
  )
