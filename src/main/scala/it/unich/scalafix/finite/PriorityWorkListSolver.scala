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

package it.unich.scalafix.finite

import it.unich.scalafix.FixpointSolverListener.EmptyListener
import it.unich.scalafix.{Assignment, FixpointSolverListener}

import scala.collection.mutable

/**
  * A fixpoint solver based on priority worklists.
  *

  */
object PriorityWorkListSolver {
  /**
    * Solve a finite equation system.
    *
    * @tparam U type of the unknowns for the equation system
    * @tparam V type of values of the equatiom system
    * @param eqs      equation system to solve
    * @param start    assignment to start the evaluation (defaults to `eqs.initial`)
    * @param ordering an ordering which specifies priorities between unknowns (defaults to the depth-first ordering
    *                 over `eqs`)
    * @param restart  at each iteration this function is applied to the new and old values. If it returns true, the
    *                 analysis of bigger unknown is restarted from the initial value. (defaults to constant `false`)
    * @param listener a listener to track the behaviour of the solver (defaults to `EmptyListener`)
    * @return the solution of the equation system
    */
  def apply[U, V](eqs: FiniteEquationSystem[U, V])
                 (start: Assignment[U, V] = eqs.initial, ordering: Ordering[U] = DFOrdering(eqs), restart: (V, V) => Boolean = { (x: V, y: V) => false },
                  listener: FixpointSolverListener[U, V] = EmptyListener): Assignment[U, V] = {
    val current = mutable.HashMap.empty[U, V].withDefault(start)
    listener.initialized(current)
    var workList = mutable.PriorityQueue.empty[U](ordering)
    workList ++= eqs.unknowns
    while (!workList.isEmpty) {
      val x = workList.dequeue()
      val newval = eqs.body(current)(x)
      listener.evaluated(current, x, newval)
      val oldval = current(x)
      if (restart(newval, oldval))
        for (y <- eqs.unknowns; if ordering.gt(y, x))
          current(y) = start(y)
      if (newval != oldval) {
        current(x) = newval
        workList ++= eqs.infl(x)
      }
    }
    listener.completed(current)
    current
  }
}
