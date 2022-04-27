/**
 * Copyright 2015, 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
 *
 * This file is part of ScalaFix. ScalaFix is free software: you can
 * redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * ScalaFix is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of a MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * ScalaFix. If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.scalafix.finite

import it.unich.scalafix.*
import it.unich.scalafix.assignments.MapBasedMutableAssignment
import it.unich.scalafix.lattice.Magma
import it.unich.scalafix.utils.Relation

/**
 * This is the abstract class for an equation system with a finite set of
 * unknowns AND static dependencies between them. When computing
 * `apply(rho)(x)`, the result may only depend on values of `rho(y)` for an `y`
 * such that `y infl x`.
 */
trait FiniteEquationSystem[U, V, EQS <: FiniteEquationSystem[U, V, EQS]] extends EquationSystem[U, V, EQS]:

  /** The collection of all unknowns. */
  val unknowns: Iterable[U]

  /** The unknowns which may be considered the input to this equation system. */
  val inputUnknowns: Set[U]

  /**
   * The static relation between an unknown x and the unknowns y it influences.
   * If `infl(x)` does not contain `y`, it means that `eqs(rho)(y) ==
   * eqs(rho')(y)`, when `rho' = rho[x / eqs(rho)(x)]`.
   */
  def infl: Relation[U]

abstract class BaseFiniteEquationSystem[U, V, EQS <: BaseFiniteEquationSystem[U, V, EQS]]
    extends BaseEquationSystem[U, V, EQS]
    with FiniteEquationSystem[U, V, EQS]:

  protected def _infl: Relation[U]

  override def infl =
    if optCombos.isEmpty || optCombos.get.combosAreIdempotent
    then _infl
    else _infl.withDiagonal

/**
 * A simple standard implementation of FiniteEquationSystem. All fields must be
 * provided explicitly by the user with the exception of `bodyWithDependencies`
 * which is computed by `body`.
 */
class SimpleFiniteEquationSystem[U, V](
    protected val _body: Body[U, V],
    protected val  _infl: Relation[U],
    val inputUnknowns: Set[U],
    val unknowns: Iterable[U]
) extends BaseFiniteEquationSystem[U, V, SimpleFiniteEquationSystem[U, V]]

object FiniteEquationSystem:
  /**
   * Returns the standard implementation of FiniteEquationSystem. All fields
   * must be provided explicitly by the user with the exception of
   * `bodyWithDependencies`.
   */
  def apply[U, V](
      body: Body[U, V],
      infl: Relation[U],
      inputUnknowns: Set[U],
      unknowns: Iterable[U]
  ): SimpleFiniteEquationSystem[U, V] =
    SimpleFiniteEquationSystem(body, infl, inputUnknowns, unknowns)
