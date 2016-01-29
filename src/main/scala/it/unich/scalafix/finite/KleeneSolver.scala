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
  * A solver based on Kleene iteration.
  */
object KleeneSolver {
  /**
    * Solve a finite equation system.
    *
    * @tparam U type of the unknowns for the equation system
    * @tparam V type of values of the equatiom system
    * @param eqs      equation system to solve
    * @param start    a assignment to start the evaluation (defaults to `eqs.initial`)
    * @param listener a listener to track the behaviour of the solver (defaults to `EmptyListener`)
    * @return the solution of the equation system
    */
  def apply[U, V](eqs: FiniteEquationSystem[U, V])
                 (start: Assignment[U, V] = eqs.initial,
                  listener: FixpointSolverListener[U, V] = EmptyListener): Assignment[U, V] = {
    var current = mutable.HashMap.empty[U, V].withDefault(start)
    val next = mutable.HashMap.empty[U, V]
    listener.initialized(current)
    var dirty = true
    while (dirty) {
      dirty = false
      for (x <- eqs.unknowns) {
        val newval = eqs.body(current)(x)
        listener.evaluated(current, x, newval)
        if (newval != current(x)) dirty = true
        next(x) = newval
      }
      current = next
    }
    listener.completed(current)
    current
  }
}
