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

import it.unich.scalafix.FixpointSolver

import scala.language.higherKinds

import it.unich.scalafix.Assignment

/**
  * A FiniteFixpointSolver is a fixpoint solver for finite equation systems.
  */
abstract class FiniteFixpointSolver extends FixpointSolver {
  /**
    * @inheritdoc
    * This solver only works with finite equation systems.
    */
  type EQS[U, V] <: FiniteEquationSystem[U, V]

  /**
    * The solve methods takes a finite equations system and parameters. If it terminates, the result is a
    * solution of the equation system.
    *
    * @param eqs    the equation system to solve
    * @param params the parameters to apply for solving the equation system
    */
  def solve[U, V](eqs: EQS[U, V], params: Params[U, V]): Assignment[U, V]
}
