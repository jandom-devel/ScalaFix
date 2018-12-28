/**
  * Copyright 2015, 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.finite

import it.unich.scalafix._
import it.unich.scalafix.lattice.Magma
import it.unich.scalafix.utils.Relation

/**
  * This is the abstract class for an equation system with a finite set of unknowns AND static dependencies between
  * them. When computing `apply(rho)(x)`, the result may only depend on values of `rho(y)` for an `y` such that
  * `y infl x`.
  */
trait FiniteEquationSystem[U, V] extends EquationSystem[U, V] {
  /**
    * The collection of all unknowns.
    */
  val unknowns: Iterable[U]

  /**
    * The unknowns which may be considered the input to this equation system.
    */
  override val inputUnknowns: Set[U]

  /**
    * The static relation between an unknown x and the unknowns y it influences. If `infl(x)` does not contain `y`, it
    * means that `eqs(rho)(y) == eqs(rho')(y)`, when `rho' = rho[x / eqs(rho)(x)]`.
    */
  val infl: Relation[U]

  override def withBoxes(boxes: BoxAssignment[U, V]): FiniteEquationSystem[U, V]

  override def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): FiniteEquationSystem[U, V]

  override def withTracer(t: EquationSystemTracer[U, V]): FiniteEquationSystem[U, V]
}

/**
  * A simple standard implementation of FiniteEquationSystem. All fields must be provided explicitly by
  * the user with the exception of `bodyWithDependencies` which is computed by `body`.
  */
case class SimpleFiniteEquationSystem[U, V]
(
  body: Body[U, V],
  initial: Assignment[U, V],
  inputUnknowns: Set[U],
  unknowns: Iterable[U],
  infl: Relation[U],
  tracer: Option[EquationSystemTracer[U, V]] = None
) extends EquationSystemBase[U, V] with FiniteEquationSystem[U,V] {

  def withBoxes(boxes: BoxAssignment[U, V]): FiniteEquationSystem[U, V] = {
    val newInfl = if (boxes.boxesAreIdempotent) infl else infl.withDiagonal
    copy(body = bodyWithBoxAssignment(boxes), infl = newInfl)
  }

  def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): FiniteEquationSystem[U, V] = {
    copy(body = bodyWithBaseAssignment(init, magma.op))
  }

  def withTracer(t: EquationSystemTracer[U, V]): FiniteEquationSystem[U, V] = {
    copy(body = bodyWithTracer(t), tracer = Some(t))
  }
}

object FiniteEquationSystem {
  /**
    * Returns the standard implementation of FiniteEquationSystem. All fields must be provided explicitly by
    * the user with the exception of `bodyWithDependencies`.
    */
  def apply[U, V](body: Body[U, V], initial: Assignment[U, V], inputUnknowns: Set[U], unknowns: Iterable[U], infl: Relation[U]): FiniteEquationSystem[U, V] =
    SimpleFiniteEquationSystem(body, initial, inputUnknowns, unknowns, infl, None)
}
