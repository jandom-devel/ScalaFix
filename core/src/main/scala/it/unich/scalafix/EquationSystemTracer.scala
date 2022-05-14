/**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it> and
 *                       Francesca Scozzari <francesca.scozzari@unich.it>
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

import java.io.PrintStream

import scala.annotation.elidable

/**
 * Implements some methods which are called by equation systems when certain
 * events occurs. They may be used for debugging, tracing, etc...
 *
 * @tparam U
 *   the type of unknowns supported by this tracer
 * @tparam V
 *   the type of values for unknowns supported by this tracer
 */
trait EquationSystemTracer[U, V]:
  /**
   * This method is called immediately before an unknown `u` is evaluated.
   *
   * @param rho
   *   the current assignment.
   * @param u
   *   the unknown which is evaluated.
   */
  def beforeEvaluation(rho: Assignment[U, V], u: U): Unit

  /**
   * This method is called immediately after an unknown `u` is evaluated.
   *
   * @param rho
   *   the current assignment.
   * @param u
   *   the unknown which is evaluated.
   * @param res
   *   the result of the evaluation.
   */
  def afterEvaluation(rho: Assignment[U, V], u: U, res: V): Unit

  /**
   * This method is called when a combo is evaluated.
   *
   * @param rho
   *   the input assignment of the body.
   * @param u
   *   the unknown to be evaluated.
   * @param res
   *   result of the evaluation of the original body.
   * @param comboed
   *   result of the evaluation of the original body, comboed with the original
   *   value.
   */
  def comboEvaluation(rho: Assignment[U, V], u: U, res: V, comboed: V): Unit

/**
 * This abstract class implements a tracer which does nothing. May be
 * sub-classed in order to override only the methods we are interested in.
 */
abstract class EquationSystemTracerAdapter[U, V] extends EquationSystemTracer[U, V]:
  /** It does nothing. */
  @elidable(TRACING)
  override def beforeEvaluation(rho: Assignment[U, V], u: U): Unit = {}

  /** It does nothing. */
  @elidable(TRACING)
  override def afterEvaluation(rho: Assignment[U, V], u: U, res: V): Unit = {}

  /** It does nothing. */
  @elidable(TRACING)
  override def comboEvaluation(rho: Assignment[U, V], u: U, res: V, comboed: V): Unit = {}

/** Collection of factory methods for equations system tracers. */
object EquationSystemTracer:

  /** An empty tracer which does nothing. */
  private final class EmptyEquationSystemTracer[U, V] extends EquationSystemTracerAdapter[U, V]

  /** A tracer which prints many debug information to a PrintStream. */
  private final class DebugEquationSystemTracer[U, V](ps: PrintStream)
      extends EquationSystemTracer[U, V]:
    @elidable(TRACING)
    override def beforeEvaluation(rho: Assignment[U, V], u: U) =
      ps.println(s"evaluated: $u oldvalue: ${rho(u)}")

    @elidable(TRACING)
    override def afterEvaluation(rho: Assignment[U, V], u: U, res: V) =
      ps.println(s"evaluated: $u oldvalue: ${rho(u)} newvalue: $res")

    @elidable(TRACING)
    override def comboEvaluation(rho: Assignment[U, V], u: U, res: V, comboed: V) =
      ps.println(s"evaluated: $u oldvalue: ${rho(u)} newvalue: $res comboed: $comboed")

  /** Instance of an empty tracer. */
  private val emptyEquationSystemTracer = EmptyEquationSystemTracer[Any, Any]

  /** Instance of a debug tracer which prints to standard output. */
  private val debugEquationSystemTracer = DebugEquationSystemTracer[Any, Any](System.out)

  /** Retuns a tracer which does nothing. */
  def empty[U, V]: EquationSystemTracer[U, V] =
    emptyEquationSystemTracer.asInstanceOf[EmptyEquationSystemTracer[U, V]]

  /** Retuns a tracer which prints many information to standard output. */
  def debug[U, V]: EquationSystemTracer[U, V] =
    debugEquationSystemTracer.asInstanceOf[DebugEquationSystemTracer[U, V]]

  /**
   * Retuns a tracer which prints many debug information to a PrintStream.
   *
   * @param ps
   *   the stream where to send the output.
   */
  def debug[U, V](ps: PrintStream): EquationSystemTracer[U, V] = DebugEquationSystemTracer[U, V](ps)
