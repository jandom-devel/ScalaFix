/**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it> and Francesca
 * Scozzari <francesca.scozzari@unich.it>
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

package it.unich.scalafix

import it.unich.scalafix.assignments.MapBasedMutableAssignment

import scala.collection.mutable

/**
 * An equation system.
 *
 * @tparam U
 *   the type for the unknowns
 * @tparam V
 *   the type for the values assumed by the unknowns.
 * @tparam EQS
 *   the type of the equation system. Operations returning a new equation system
 *   generally return `EQS`.
 */
trait EquationSystem[U, V, EQS <: EquationSystem[U, V, EQS]]:

  /** Returns the body of this equation system. */
  def body: Body[U, V]

  /**
   * Returns the body with dependencies of the equations system. Although this
   * might be implemented by [[scalafix#withDependencies]], it is possible to
   * provide an alternative implementation.
   */
  def bodyWithDependencies: BodyWithDependencies[U, V]

  /**
   * Returns a mutable assignment based on the provided initial assignment.
   * Although hash maps are the most obvious implementation of mutable
   * assignment, it is possible to provide an alternative implementation.
   *
   * @param rho
   *   the initial assignment.
   */
  def getMutableAssignment(rho: Assignment[U, V]): MutableAssignment[U, V]

  /**
   * Returns the equation system modified with the specified combo assignment.
   *
   * @param combos
   *   the combo assignment for the equation system.
   * @see
   *   [[scalafix#addCombos]]
   */
  def withCombos(combos: ComboAssignment[U, V]): EQS

  /**
   * Returns the equation system modified with the specified base assignment.
   *
   * @param baseAssignment
   *   a partial assignment of values to unknowns.
   * @param op
   *   the operation used to combine the base assignment with the rhs of the
   *   body.
   * @see
   *   [[scalafix#addBaseAssignment]]
   */
  def withBaseAssignment(baseAssignment: PartialFunction[U, V], op: (V, V) => V): EQS

  /**
   * Returns the equation system with an additional tracer. The tracer contains
   * callbacks which are invoked during body evaluation.
   *
   * @param tracer
   *   the tracer.
   */
  def withTracer(tracer: EquationSystemTracer[U, V]): EQS

/**
 * The base abstract implementation of an equation systems.
 *
 * @tparam U
 *   the type for the unknowns
 * @tparam V
 *   the type for the values assumed by the unknowns.
 * @tparam EQS
 *   the type of the equation system. Operations returning a new equation system
 *   generally return `EQS`.
 */
abstract class BaseEquationSystem[U, V, EQS <: BaseEquationSystem[U, V, EQS]]
    extends EquationSystem[U, V, EQS]
    with Cloneable:

  /**
   * The initial body of the equation system. Depending of the presence of
   * combos, base assignments or tracers, the initial body is manipulated in
   * order to obtain the real body of the equation system.
   */
  protected def initialBody: Body[U, V]

  /** An optional assignment of combos to the unknowns. */
  protected var optCombos: Option[ComboAssignment[U, V]] = None

  /**
   * An optional specification for a base assignment we want to add to the
   * equation system.
   */
  protected var optBaseAssignment: Option[(PartialFunction[U, V], (V, V) => V)] = None

  /** An optional tracer for monitoring the execution of the body. */
  protected var optTracer: Option[EquationSystemTracer[U, V]] = None

  /**
   * A type-aware clone operation. An equation system is immutable as long as
   * the public interface is concerned, but cloning is used to simplify
   * implementation.
   */
  override protected def clone(): EQS =
    super.clone().asInstanceOf[EQS]

  /**
   * @inheritdoc
   *
   * @note
   *   The body is generated starting from the initial body of the equation
   *   system, adding combos, base assignment and tracer according to the
   *   requests of the user.
   */
  override def body: Body[U, V] =
    val basedBody = optBaseAssignment match
      case Some(baseAssignment, op) => initialBody.addBaseAssignment(baseAssignment, op)
      case None                     => initialBody
    val comboedBody = optCombos match
      case Some(combos) => basedBody.addCombos(combos, optTracer)
      case None         => basedBody
    optTracer match
      case Some(tracer) => comboedBody.addTracer(tracer)
      case None         => comboedBody

  /**
   * Returns the body with dependencies of the equations system. It is
   * implemented by calling [[scalafix#withDependencies]].
   */
  override def bodyWithDependencies = body.withDependencies

  /** @inheritdoc */
  override def withCombos(combos: ComboAssignment[U, V]) =
    val clone = this.clone()
    clone.optCombos = Some(combos)
    clone

  /** @inheritdoc */
  override def withBaseAssignment(
      baseAssignment: PartialFunction[U, V],
      op: (V, V) => V
  ) =
    val clone = this.clone()
    clone.optBaseAssignment = Some(baseAssignment, op)
    clone

  /** @inheritdoc */
  override def withTracer(tracer: EquationSystemTracer[U, V]) =
    val clone = this.clone()
    clone.optTracer = Some(tracer)
    clone

  /**
   * Returns a mutable assignment based on the provided initial assignment.
   *
   * @note
   *   Returns the default mutable assignment
   *   [[assignments.MapBasedMutableAssignment]]
   */
  override def getMutableAssignment(rho: Assignment[U, V]) = MapBasedMutableAssignment(rho)

/**
 * Default implementation of an equation system.
 *
 * @param initialBody
 *   the initial body of the equation system. Depending of the presence of
 *   combos, base assignments or tracers, the initial body is manipulated in
 *   order to obtain the real body of the equation system.
 */
class SimpleEquationSystem[U, V](
    protected val initialBody: Body[U, V]
) extends BaseEquationSystem[U, V, SimpleEquationSystem[U, V]]

/** Collection of factory methods for equation systems. */
object EquationSystem:
  /**
   * Returns the standard implementation of an EquationSystem.
   *
   * @param initialBody
   *   the initial body of the equation system. Depending of the presence of
   *   combos, base assignments or tracers, the initial body is manipulated in
   *   order to obtain the real body of the equation system.
   */
  def apply[U, V](initialBody: Body[U, V]): SimpleEquationSystem[U, V] =
    SimpleEquationSystem(initialBody)
