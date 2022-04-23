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
import it.unich.scalafix.assignments.*

/** A solver based on Kleene iteration. */
object KleeneSolver:
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
   *   a listener to track the behaviour of the solver (defaults to the empty
   *   tracer)
   * @return
   *   the solution of the equation system
   */
  def apply[U, V](eqs: FiniteEquationSystem[U, V])(
      start: InputAssignment[U, V],
      tracer: FixpointSolverTracer[U, V] = FixpointSolverTracer.empty[U, V]
  ): OutputAssignment[U, V] =
    var current = start.toMutableAssignment
    var next = start.toMutableAssignment
    tracer.initialized(current)
    var dirty = true
    while dirty do
      dirty = false
      for x <- eqs.unknowns do
        val newval = eqs.body(current)(x)
        tracer.evaluated(current, x, newval)
        if newval != current(x) then dirty = true
        next(x) = newval
      val temp = current
      current = next
      next = temp
    tracer.completed(current)
    current.toOutputAssignment
