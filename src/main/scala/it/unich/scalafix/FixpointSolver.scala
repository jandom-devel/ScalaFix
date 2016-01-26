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

package it.unich.scalafix

import scala.language.higherKinds

/**
  * This is the common ancestor for all fixpoint solvers of equation systems. It is just a marker trait.
  * All fixpoint solvers have different apply methods (with different parameters) which may be used
  * to solve an equation system.
  */
abstract class FixpointSolver {

  /**
    * Each fixpoint solver needs an equation system and some parameters. Parameters are generally different
    * for each fixpoint solver, hence they are provided as an inner type. Each type of parameter should
    * inherit from BaseParams.
    */
  abstract protected class BaseParams[U, V] {
    /**
      * The initial assignment to start the analysis.
      */
    val start: Assignment[U, V]

    /**
      * A listener for debugging or tracing purposes.
      */
    val listener: FixpointSolverListener[U, V]
  }

  /**
    * The type of parameters supported by a given fixpoint solver.
    */
  type Params[U, V] <: BaseParams[U, V]

  /**
    * This is the subclass of equation systems the solver may work with.
    */
  type EQS[U, V] <: EquationSystem[U, V]
}

object FixpointSolver {

}