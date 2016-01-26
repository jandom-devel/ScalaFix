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

import it.unich.scalafix._
import it.unich.scalafix.lattice.Magma
import it.unich.scalafix.utils.{Relation, IterableFunction}

/**
  * This is the abstract class for an equation system with a finite set of unknowns AND static dependencies between
  * them. When computing `apply(rho)(x)`, the result may only depend on values of `rho(y)` for an `y` such that
  * `y infl x`.
  */
abstract class FiniteEquationSystem[U, V] extends EquationSystem[U, V] {
  /**
    * The collection of all unknowns.
    */
  val unknowns: Iterable[U]

  /**
    * The unknowns which may be considered the input to this equation system.
    */
  val inputUnknowns: Set[U]

  /**
    * The static relation between an unknown x and the unknowns y it influences. If `infl(x)` does not contain `y`, it
    * means that `eqs(rho)(y) == eqs(rho')(y)`, when `rho' = rho[x / eqs(rho)(x)]`.
    */
  val infl: Relation[U]

  /**
    * Add boxes to the equation system.
    */
  def withBoxes(boxes: BoxAssignment[U, V]): FiniteEquationSystem[U, V]

  /**
    * Combine a base assignment with the equation system
    *
    * @param init the assignment to add to the equation system
    */
  def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): FiniteEquationSystem[U, V]
}

object FiniteEquationSystem {

  /**
    * A trait which provides the withBaseAssignment method to finite equation systems.
    */
  trait WithBaseAssignment[U, V] {
    this: FiniteEquationSystem[U, V] =>

    def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): FiniteEquationSystem[U, V] =
      new SimpleFiniteEquationSystem(
        body = body.withBaseAssignment(init),
        initial = initial,
        inputUnknowns = inputUnknowns,
        unknowns = unknowns,
        infl = infl
      )
  }

  /**
    * A trait which provides the withBoxes method to finite equation systems.
    */
  trait WithBoxes[U, V] {
    this: FiniteEquationSystem[U, V] =>

    def withBoxes(boxes: BoxAssignment[U, V]): FiniteEquationSystem[U, V] =
      new SimpleFiniteEquationSystem(
        body = body.withBoxAssignment(boxes),
        initial = initial,
        inputUnknowns = inputUnknowns,
        unknowns = unknowns,
        infl = if (boxes.areIdempotent) infl else infl.withDiagonal
      )
  }

  /**
    * A class defining a finite equation system given its constituents parts (with the excpetion of the body with
    * dependencies which is automatically computed).
    */
  final class SimpleFiniteEquationSystem[U, V](
                                                val body: Body[U, V],
                                                val initial: Assignment[U, V],
                                                val inputUnknowns: Set[U],
                                                val unknowns: Iterable[U],
                                                val infl: Relation[U]
                                              )
    extends FiniteEquationSystem[U, V] with EquationSystem.BodyWithDependenciesFromBody[U, V] with WithBaseAssignment[U, V] with WithBoxes[U, V]

  /**
    * A finite equation system given its constituents parts (with the exception of the body with dependencies which
    * is automatically computed).
    */
  def apply[U, V](body: Body[U, V], initial: Assignment[U, V], inputUnknowns: Set[U], unknowns: Iterable[U], infl: Relation[U]): FiniteEquationSystem[U, V] =
    new SimpleFiniteEquationSystem(body, initial, inputUnknowns, unknowns, infl)
}
