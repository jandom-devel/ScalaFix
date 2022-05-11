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

/**
* This object cntains many enumerations and auxiliary classes which are used by
* the high-level fixpoint solver.
*/
object Parameters:

  /** The solvers supported by this driver. */
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
