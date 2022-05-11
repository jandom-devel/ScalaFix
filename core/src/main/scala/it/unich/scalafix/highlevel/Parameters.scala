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

package it.unich.scalafix.highlevel

import it.unich.scalafix.*

/** The solvers supported by the high level fixpoint solvers. */
enum Solver:
  case KleeneSolver, RoundRobinSolver, PriorityWorkListSolver, WorkListSolver,
    HierarchicalOrderingSolver

/** The strategies for applying widening and arrowings. */
enum ComboStrategy:
  /** Only apply widenings. */
  case OnlyWidening

  /** Standard two pass widenings/narrowings iteration. */
  case TwoPhases

  /** Single pass with a warrowing. */
  case Warrowing

/** The way in which widening should be applied at the chosen unknowns. */
enum ComboScope:
  /** Standard widening usage. */
  case Standard

  /** Localized widening. */
  case Localized

/** Locations where to put widenings and narrowings. */
enum ComboLocation:

  /** Put widenings/narrowings nowhere. */
  case None

  /** Put widenings/narrowings at each unknown. */
  case All

  /** Put widenings/narrowings at each loop head. */
  case Loop

/** Restarting strategy to adopt. */
enum RestartStrategy:

  /** Do not apply a restart strategy. */
  case None

  /** Apply a restart strategy. */
  case Restart

/**
 * Parameters for the commodity solvers in the [[highlevel]] package.
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
case class Parameters[U, V](
    solver: Solver,
    start: Assignment[U, V],
    comboLocation: ComboLocation = ComboLocation.Loop,
    comboScope: ComboScope = ComboScope.Standard,
    comboStrategy: ComboStrategy = ComboStrategy.TwoPhases,
    restartStrategy: RestartStrategy = RestartStrategy.None,
    widenings: ComboAssignment[U, V] = ComboAssignment.empty,
    narrowings: ComboAssignment[U, V] = ComboAssignment.empty,
    tracer: FixpointSolverTracer[U, V] = FixpointSolverTracer.empty
)

object Parameters {

  /**
   * Returns parameters for solving an equation system with the standard CC77
   * approach based on an ascending chain with widening and a descending chain with
   * narrowing.
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
  ): Parameters[U, V] =
    Parameters[U, V](
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

}
