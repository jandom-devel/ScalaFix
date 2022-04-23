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

package it.unich.scalafix.infinite

import scala.collection.mutable

import it.unich.scalafix.*
import it.unich.scalafix.assignments.*

/** A local fixpoint solver based on a worklist. */
object WorkListSolver:
  /**
   * Locally solve a finite equation system.
   *
   * @tparam U
   *   type of the unknowns for the equation system
   * @tparam V
   *   type of values of the equatiom system
   * @param eqs
   *   equation system to solve
   * @param wanted
   *   the unknowns we want to solve
   * @param start
   *   assignment to start the evaluation
   * @param tracer
   *   a tracer to track the behaviour of the solver (defaults to the empty
   *   tracer)
   * @return
   *   the solution of the equation system
   */
  def apply[U, V](eqs: EquationSystem[U, V])(
      start: Assignment[U, V],
      wanted: Iterable[U],
      tracer: FixpointSolverTracer[U, V] = FixpointSolverTracer.empty[U, V]
  )(using factory: MutableAssignmentFactory[U, V]): MutableAssignment[U, V] =
    val infl = mutable.Map.empty[U, mutable.Set[U]]
    val workList = mutable.Queue.empty[U]
    workList ++= wanted

    val current = factory(start)
    tracer.initialized(current)
    while workList.nonEmpty do
      val x = workList.dequeue()
      val (newval, dependencies) = eqs.bodyWithDependencies(current)(x)
      tracer.evaluated(current, x, newval)
      for y <- dependencies do
        if !current.isDefinedAt(y) then
          current(y) = start(y)
          workList += y
        infl.getOrElseUpdate(y, mutable.Set.empty[U]) += x
      if newval != current(x) then
        current(x) = newval
        workList ++= infl(x)
    tracer.completed(current)
    current
