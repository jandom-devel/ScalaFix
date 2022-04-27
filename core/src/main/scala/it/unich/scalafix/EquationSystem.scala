/**
 * Copyright 2015, 2016, 2017, 2022 Gianluca Amato <gianluca.amato@unich.it>
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
import it.unich.scalafix.lattice.Magma

import scala.collection.mutable

/**
 * This is the innterface a generic equation system.
 *
 * @tparam U
 *   the type for the unknowns of this equation system.
 * @tparam V
 *   the type for the values assumed by the unknowns of this equation system.
 */
trait EquationSystem[U, V, EQS <: EquationSystem[U, V, EQS]]:

  /** Returns the body of the equation system. */
  def body: Body[U, V]

  /**
   * Returns the body (with dependencies) of the equations system. Although this
   * might be implemented as `body.withDependencies`, it is possible, this
   * method allows to provide an alternative implementation.
   */
  def bodyWithDependencies: BodyWithDependencies[U, V]

  /**
   * Returns a mutable assignment based on the provided initial assignment.
   *
   * @param rho
   *   the initial assignment
   */
  def getMutableAssignment(rho: Assignment[U, V]): MutableAssignment[U, V]

  /**
   * Returns the equation system modified with the specified combo assignment.
   *
   * @param combos
   *   the combo assignment for the equation system
   */
  def withCombos(combos: ComboAssignment[U, V]): EQS

  /**
   * Returns the equation system modified with the specified base assignment.
   * The `baseAssignment` is combined with the result of body evaluation trough
   * the use of the implicit operator of the magma `V`.
   *
   * @param baseAssignment
   *   the assignment for the equation system
   */
  def withBaseAssignment(baseAssignment: PartialFunction[U, V])(using Magma[V]): EQS =
    withBaseAssignment(baseAssignment, _ op _)

  /**
   * Returns the same equation system modified with the specified base
   * assignment. The `baseAssignment` is combined with the result of body
   * evaluation trough the use of the implicit operator of the magma `V`.
   *
   * @param init
   *   the assignment to add to the equation system
   * @param op
   *   the operation used to combine the initial assignment and the result of
   *   the body evaluation
   */
  def withBaseAssignment(init: PartialFunction[U, V], op: (V, V) => V): EQS

  /**
   * Returns the same equation system with an additional tracer. The tracer
   * contains call-backs to be invoked during body evaluation.
   *
   * @param t
   *   the tracer
   */
  def withTracer(t: EquationSystemTracer[U, V]): EQS

abstract class BaseEquationSystem[U, V, EQS <: BaseEquationSystem[U, V, EQS]]
    extends EquationSystem[U, V, EQS]
    with Cloneable:

  protected def _body: Body[U, V]
  protected var optCombos: Option[ComboAssignment[U, V]] = None
  protected var optBaseAssignment: Option[PartialFunction[U, V]] = None
  protected var optOp: Option[(V, V) => V] = None
  protected var optTracer: Option[EquationSystemTracer[U, V]] = None

  override def clone(): EQS =
    super.clone().asInstanceOf[EQS]

  override def body: Body[U, V] =
    val basedBody =
      if optBaseAssignment.isDefined && optOp.isDefined
      then _body.addBaseAssignment(optBaseAssignment.get, optOp.get)
      else _body
    val comboedBody =
      if optCombos.isDefined
      then basedBody.addCombos(optCombos.get, optTracer)
      else basedBody
    if optTracer.isDefined then comboedBody.addTracer(optTracer.get) else comboedBody

  override def bodyWithDependencies = body.withDependencies

  override def withCombos(combos: ComboAssignment[U, V]) =
    val clone = this.clone()
    clone.optCombos = Some(combos)
    clone

  override def withBaseAssignment(
      baseAssignment: PartialFunction[U, V],
      op: (V, V) => V
  ) =
    val clone = this.clone()
    clone.optBaseAssignment = Some(baseAssignment)
    clone.optOp = Some(op)
    clone

  override def withTracer(tracer: EquationSystemTracer[U, V]) =
    val clone = this.clone()
    clone.optTracer = Some(tracer)
    clone

  /**
   * Returns the default mutable assignment, i.e. a
   * [[it.unich.scalafix.assignments.MapBasedMutableAssignment]].
   */
  override def getMutableAssignment(rho: Assignment[U, V]) = MapBasedMutableAssignment(rho)

class SimpleEquationSystem[U, V](
    protected val _body: Body[U, V],
) extends BaseEquationSystem[U, V, SimpleEquationSystem[U, V]]

object EquationSystem:
  /**
   * Returns the standard implementation of EquationSystem. All fields must be
   * provided explicitly by the user with the exception of
   * `bodyWithDependencies`.
   */
  def apply[U, V](body: Body[U, V]): SimpleEquationSystem[U, V] = SimpleEquationSystem(body)
