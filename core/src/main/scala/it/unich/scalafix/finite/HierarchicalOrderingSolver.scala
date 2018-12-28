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
  * A solver whose strategy in based on a hierarchical ordering.
  */
object HierarchicalOrderingSolver {
  /**
    * Solve a finite equation system.
    *
    * @tparam U type of the unknowns for the equation system
    * @tparam V type of values of the equatiom system
    * @param eqs      equation system to solve
    * @param start    assignment to start the evaluation (defaults to `eqs.initial`)
    * @param ordering a hierarchical ordering which specifies priorities between unknowns (defaults to the
    *                 hierarchical ordering induce by the depth-first ordering over `eqs`)
    * @param listener a listener to track the behaviour of the solver (defaults to `EmptyListener`)
    * @return the solution of the equation system
    */
  def apply[U, V](eqs: FiniteEquationSystem[U, V])
                 (start: Assignment[U, V], ordering: HierarchicalOrdering[U] = HierarchicalOrdering(DFOrdering(eqs)),
                  listener: FixpointSolverListener[U, V] = EmptyListener): Assignment[U, V] = {
    import HierarchicalOrdering._

    val current = mutable.HashMap.empty[U, V].withDefault(start)
    listener.initialized(current)
    val stack = mutable.Stack.empty[Int]
    val stackdirty = mutable.Stack.empty[Boolean]

    var dirty = false
    var i = 0
    val sequence = ordering.toSeqWithParenthesis

    while (i < sequence.length) {
      sequence(i) match {
        case Left =>
          stack.push(i + 1)
          stackdirty.push(dirty)
          dirty = false
          i += 1
        case Val(x) =>
          val newval = eqs.body(current)(x)
          listener.evaluated(current, x, newval)
          if (newval != current(x)) {
            current(x) = newval
            dirty = true
          }
          i += 1
        case Right =>
          if (dirty) {
            i = stack.top
            dirty = false
          } else {
            stack.pop
            dirty = stackdirty.pop()
            i += 1
          }
      }
    }
    listener.completed(current)
    current
  }
}
