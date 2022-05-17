 /**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it> and
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
import it.unich.scalafix.assignments.*

/** A fixpoint solver based on a worklist. */
object WorkListSolver:
  /**
   * Solve a finite equation system.
   *
   * @tparam U
   *   type of the unknowns for the equation system
   * @tparam V
   *   type of values of the equatiom system
   * @param eqs
   *   equation system to solve
   * @param start
   *   a assignment to start the evaluation
   * @param tracer
   *   a tracer to track the behaviour of the solver (defaults to the empty
   *   tracer)
   * @return
   *   the solution of the equation system
   */
  def apply[U, V](eqs: FiniteEquationSystem[U, V, ?])(
      start: Assignment[U, V],
      tracer: FixpointSolverTracer[U, V] = FixpointSolverTracer.empty[U, V]
  ): MutableAssignment[U, V] =
    val current = eqs.getMutableAssignment(start)
    val eqsUnknowns = eqs.unknowns
    val eqsBodyCurrent = eqs.body(current)
    val eqsInfl = eqs.infl
    tracer.initialized(current)
    // is it better to use a Queue for a worklist ?
    val workList = collection.mutable.LinkedHashSet.empty[U]
    workList ++= eqsUnknowns
    while workList.nonEmpty do
      val x = workList.head
      workList.remove(x)
      val newval = eqsBodyCurrent(x)
      tracer.evaluated(current, x, newval)
      if newval != current(x) then
        current(x) = newval
        // variant with Queue
        // for (y <- eqs.infl(x); if !(workList contains y)) workList += y
        workList ++= eqsInfl(x)
    tracer.completed(current)
    current
