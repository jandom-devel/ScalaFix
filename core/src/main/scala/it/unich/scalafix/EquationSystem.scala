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

package it.unich.scalafix

import it.unich.scalafix.lattice.Magma

import scala.collection.mutable

/**
  * The body of an equation system, i.e., a map from assignments to assignments.
  */
type Body[U, V] = Assignment[U, V] => Assignment[U, V]

/**
  * A body which also calculates dependencies among unknowns.
  */
type BodyWithDependencies[U, V] = Assignment[U, V] => Assignment[U, (V, Iterable[U])]

/**
  * This is the abstract class for a generic equation system.
  *
  * @tparam U the type for the unknowns of this equation system.
  * @tparam V the type for the values assumed by the unknowns of this equation system.
  */
trait EquationSystem[U, V]:
  /**
    * The body of the equation system, i.e., a map `Assignment[U,V] => Assignment[U,V]`.
    */
  val body: Body[U, V]

  /**
    * Given an assignment `rho` and unknown `u`, returns the pair `(body(rho)(x), uks)`. `uks` is a set of unknowns
    * with the property that if `rho'` differs from `rho` only for variables which are not in `uks`, then
    * `body(rho)(u)==body(rho')(u)`.
    */
  val bodyWithDependencies: BodyWithDependencies[U, V]

  /**
    * An initial value for starting the analyzer
    */
  val initial: Assignment[U, V]

  /**
    * The unknowns which may be considered the input to this equation system.
    */
  val inputUnknowns: U => Boolean

  /**
    * Add boxes to the equation system
    *
    * @param boxes a box assignment
    */
  def withBoxes(boxes: BoxAssignment[U, V]): EquationSystem[U, V]

  /**
    * Combine a base assignment with the equation system. The type `V` should be endowed with a magma.
    *
    * @param init the assignment to add to the equation system
    */
  def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): EquationSystem[U, V]

  /**
    * Add a tracer to the equation system. The tracer contains call-backs to be invoked during body evaluation.
    *
    * @param t the tracer
    */
  def withTracer(t: EquationSystemTracer[U, V]): EquationSystem[U, V]

/**
  * This class implements common utility methods.
  */
abstract class EquationSystemBase[U, V] extends EquationSystem[U, V]:
  /**
    * An optional tracer which should be called during body evaluation.
    */
  protected val tracer: Option[EquationSystemTracer[U, V]]

  /**
    * Returns a new body which calls the current tracer before and after evaluation.
    *
    * @param t the tracer to be called by the new body
    */
  protected def bodyWithTracer(t: EquationSystemTracer[U, V]): Body[U, V] =
    (rho: Assignment[U, V]) =>
      (x: U) =>
        t.beforeEvaluation(rho, x)
        val res = body(rho)(x)
        t.afterEvaluation(rho, x, res)
        res

  /**
    * Returns a new body with boxes added to the evaluation. If `tracer` is defined, the `boxEvaluation`
    * callback is invoked during evaluation.
    */
  protected def bodyWithBoxAssignment(boxes: BoxAssignment[U, V]): Body[U, V] =
    val realBoxes = boxes.copy
    if realBoxes.isEmpty then
      body
    else
      (rho: Assignment[U, V]) =>
        (x: U) =>
          val res = body(rho)(x)
          if realBoxes.isDefinedAt(x) then
            val boxedRes = realBoxes(x)(rho(x), res)
            tracer foreach (_.boxEvaluation(rho, x, res, boxedRes))
            boxedRes
          else
            res

  /**
    * Returns a new body, in which the `init` assignment is combined with the result of body evaluation
    * trough the use of the `comb` combiner.
    */
  protected def bodyWithBaseAssignment(init: PartialFunction[U, V], comb: (V, V) => V): Body[U, V] =
    (rho: Assignment[U, V]) => (x: U) => if init.isDefinedAt(x) then comb(init(x), body(rho)(x)) else body(rho)(x)

  /**
    * Implement the `bodyWithDependencies` method by instrumenting the source assignment in order to
    * record access to unknowns.
    */
  val bodyWithDependencies: BodyWithDependencies[U, V] =
    (rho: Assignment[U, V]) =>
      (x: U) => {
        val queried = mutable.Buffer.empty[U]
        val trackrho = { (y: U) =>
          queried.append(y)
          rho(y)
        }
        val newval = body(trackrho)(x)
        (newval, queried)
      }

/**
  * A simple standard implementation of EquationSystem. All fields must be provided explicitly by
  * the user with the exception of `bodyWithDependencies` which is computed by `body`.
  */
case class SimpleEquationSystem[U, V]
(
  body: Body[U, V],
  initial: Assignment[U, V],
  inputUnknowns: U => Boolean,
  tracer: Option[EquationSystemTracer[U, V]] = None
) extends EquationSystemBase[U, V]:
  def withBoxes(boxes: BoxAssignment[U, V]): EquationSystem[U, V] =
    copy(body = bodyWithBoxAssignment(boxes))

  def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): EquationSystem[U, V] =
    copy(body = bodyWithBaseAssignment(init, magma.op))

  def withTracer(t: EquationSystemTracer[U, V]): EquationSystem[U, V] =
    copy(body = bodyWithTracer(t), tracer = Some(t))

object EquationSystem:
  /**
    * Returns the standard implementation of EquationSystem. All fields must be provided explicitly by
    * the user with the exception of `bodyWithDependencies`.
    */
  def apply[U, V](body: Body[U, V], initial: Assignment[U, V], inputUnknowns: U => Boolean = { (_: U) => false }): EquationSystem[U, V] =
    SimpleEquationSystem(body, initial, inputUnknowns, None)
