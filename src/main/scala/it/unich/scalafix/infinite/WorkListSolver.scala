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

package it.unich.scalafix.infinite

import scala.collection.mutable
import it.unich.scalafix.{FixpointSolverListener, EquationSystem}

/**
  * A local fixpoint solver based on a worklist.
  */
object WorkListSolver extends LocalFixpointSolver {

  /**
    * Parameters needed for the local worklist solver
    *
    * @param start    the initial assignment.
    * @param wanted   the collection of unknowns for which we want a solution.
    * @param listener the listener whose callbacks are invoked for debugging and tracing.
    */
  case class Params[U, V](start: U => V,
                          wanted: Iterable[U],
                          listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener
                         ) extends LocalBaseParams[U, V]

  type EQS[U, V] = EquationSystem[U, V]

  def solve[U, V](eqs: EquationSystem[U, V], params: Params[U, V]) = {
    import params._

    val infl = new mutable.HashMap[U, mutable.Set[U]] with mutable.MultiMap[U, U] {
      override def makeSet = new mutable.LinkedHashSet[U]
    }
    var workList = mutable.Queue.empty[U]
    workList ++= wanted

    val current = mutable.HashMap.empty[U, V].withDefault(start)
    listener.initialized(current)
    while (!workList.isEmpty) {
      val x = workList.dequeue()
      val (newval, dependencies) = eqs.bodyWithDependencies(current)(x)
      listener.evaluated(current, x, newval)
      for (y <- dependencies) {
        if (!current.isDefinedAt(y)) {
          current(y) = start(y)
          workList += y
        }
        infl.addBinding(y, x)
      }
      if (newval != current(x)) {
        current(x) = newval
        workList ++= infl(x)
      }
    }
    current
  }

  /**
    * A convenience method for calling the solver
    */
  def apply[U, V](eqs: EquationSystem[U, V], start: U => V, wanted: Iterable[U], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) =
    solve(eqs, Params(start, wanted, listener))
}
