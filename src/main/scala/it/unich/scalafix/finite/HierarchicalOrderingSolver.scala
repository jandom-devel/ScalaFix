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

import scala.collection.mutable
import it.unich.scalafix.Assignment
import it.unich.scalafix.FixpointSolverListener

/**
  * A solver whose strategy in based on a hierarchical ordering.
  */
object HierarchicalOrderingSolver extends FiniteFixpointSolver {

  /**
    * Parameters needed for the hierarchical ordering solver
    *
    * @param start    the initial assignment.
    * @param ordering the hierarchical ordering which drives the analysis.
    * @param listener the listener whose callbacks are invoked for debugging and tracing.
    */
  case class Params[U, V](start: Assignment[U, V], ordering: HierarchicalOrdering[U], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) extends BaseParams[U, V]

  type EQS[U, V] = FiniteEquationSystem[U, V]

  def solve[U, V](eqs: EQS[U, V], params: Params[U, V]) = {
    import HierarchicalOrdering._
    import params._

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
    current
  }

  /**
    * A convenience method for calling the solver
    */
  def apply[U, V](eqs: EQS[U, V], start: Assignment[U, V], ordering: HierarchicalOrdering[U], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) =
    solve(eqs, Params(start, ordering, listener))
}
