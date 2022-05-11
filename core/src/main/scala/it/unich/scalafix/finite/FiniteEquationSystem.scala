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
import it.unich.scalafix.utils.Relation

/**
 * An equation system with a finite set of unknowns AND static dependencies
 * between them. When computing `apply(rho)(x)`, the result may only depend on
 * `rho(y)` if `y` is in `inlf(x)`.
 *
 * @tparam U
 *   the type for the unknowns
 * @tparam V
 *   the type for the values assumed by the unknowns.
 * @tparam EQS
 *   the type of the equation system. Operations returning a new equation system
 *   generally return `EQS`.
 */
trait FiniteEquationSystem[U, V, EQS <: FiniteEquationSystem[U, V, EQS]]
    extends EquationSystem[U, V, EQS]:

  /** The collection of all unknowns. */
  val unknowns: Iterable[U]

  /** The unknowns which may be considered the input to this equation system. */
  val inputUnknowns: Set[U]

  /**
   * The static relation between an unknown `x` and the unknowns `y` it
   * influences. If `infl(x)` does not contain `y`, it means that `apply(rho)(y)
   * \== apply(rho')(y)`, when `rho' = rho[x / eqs(rho)(x)]`.
   */
  def infl: Relation[U, U]

/**
 * The base abstract implementation for finite equation systems.
 *
 * @tparam U
 *   the type for the unknowns
 * @tparam V
 *   the type for the values assumed by the unknowns.
 * @tparam EQS
 *   the type of the equation system. Operations returning a new equation system
 *   generally return `EQS`.
 */
abstract class BaseFiniteEquationSystem[U, V, EQS <: BaseFiniteEquationSystem[U, V, EQS]]
    extends BaseEquationSystem[U, V, EQS]
    with FiniteEquationSystem[U, V, EQS]:

  /**
   * The initial influence relation of the equation system. Depending of the
   * presence of combos, the initial relaton is manipulated in order to obtain
   * the real influence relation of the equation system.
   */
  protected def initialInfl: Relation[U, U]

  /** @inheritdoc */
  override def infl = optCombos match
    case Some(combos) if ! combos.combosAreIdempotent => initialInfl.withDiagonal
    case _ => initialInfl
/**
 * Default implementation of a finite equation system.
 *
 * @param initialBody
 *   the initial body of the equation system. Depending of the presence of
 *   combos, base assignments or tracers, the initial body is manipulated in
 *   order to obtain the real body of the equation system.
 * @param initialInfl
 *   the initial influence relation of the equation system. Depending of the
 *   presence of combos, the initial relaton is manipulated in order to obtain
 *   the real influence relation of the equation system.
 * @param unknowns
 *   collection of all unknowns.
 * @param inputUnknowns
 *   the unknowns which may be considered the input to this equation system.
 */
class SimpleFiniteEquationSystem[U, V](
    protected val initialBody: Body[U, V],
    protected val initialInfl: Relation[U, U],
    val unknowns: Iterable[U],
    val inputUnknowns: Set[U]
) extends BaseFiniteEquationSystem[U, V, SimpleFiniteEquationSystem[U, V]]

/** Collection of factory methods for finite equation systems. */
object FiniteEquationSystem:
  /**
   * Returns the standard implementation of a finite equation system.
   *
   * @see
   *   [[SimpleFiniteEquationSystem]] for the meaning of all the parameters.
   */
  def apply[U, V](
      initialBody: Body[U, V],
      initialInfl: Relation[U, U],
      unknowns: Iterable[U],
      inputUnknowns: Set[U]
  ): SimpleFiniteEquationSystem[U, V] =
    SimpleFiniteEquationSystem(initialBody, initialInfl, unknowns, inputUnknowns)
