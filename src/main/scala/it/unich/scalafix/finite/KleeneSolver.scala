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

import it.unich.scalafix.Assignment
import it.unich.scalafix.FixpointSolverListener

import scala.collection.mutable

/**
  * A solver based on Kleene iteration.
  */
object KleeneSolver extends FiniteFixpointSolver {

  /**
    * Parameters needed for the Kleene solver
    *
    * @param start    the initial assignment.
    * @param listener the listener whose callbacks are invoked for debugging and tracing.
    */
  case class Params[U, V](
                           start: Assignment[U, V],
                           listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener
                         ) extends BaseParams[U, V]

  /**
    * @inheritdoc
    * This solver only works with finite equation systems.
    */
  type EQS[U, V] = FiniteEquationSystem[U, V]

  def solve[U, V](eqs: EQS[U, V], params: Params[U, V]) = {
    import params._

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

  /**
    * A convenience method for calling the solver
    */
  def apply[U, V](
                   eqs: EQS[U, V],
                   start: Assignment[U, V],
                   listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener
                 ) =
    solve(eqs, Params(start, listener))
}
