/**
  * Copyright 2017 Gianluca Amato <gianluca.amato@unich.it>
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

import java.io.PrintStream

import scala.annotation.elidable

/**
  * An EquationSystemTracer implements some methods which are called by equation systems when certain
  * events occurs. They may be used for debugging, tracing, etc...
  *
  * @tparam U the type of unknowns supported by this tracer
  * @tparam V the type of values for unknowns supported by this tracer
  */
trait EquationSystemTracer[U, V]:
  /**
    * This method is called immediately before an unknown `u` is evaluated.
    *
    * @param rho the current assignment
    * @param u   the unknown which is evaluated
    */
  def beforeEvaluation(rho: Assignment[U, V], u: U): Unit

  /**
    * This method is called immediately after an unknown `u` is evaluated.
    *
    * @param rho the current assignment
    * @param u   the unknown which is evaluated
    * @param res the result of the evaluation
    */
  def afterEvaluation(rho: Assignment[U, V], u: U, res: V): Unit

  /**
    * This method is called when a box is evaluated.
    *
    * @param rho   the input assignment of the body
    * @param u     the unknown to be evaluated
    * @param res   result of the evaluation of the original body
    * @param boxed result of the evaluation of the original body, boxed with the original value
    */
  def boxEvaluation(rho: Assignment[U, V], u: U, res: V, boxed: V): Unit

/**
  * This abstract class implements a tracer which does nothing.
  * May be sub-classed in order to override only the methods we are interested in.
  */
abstract class EquationSystemTracerAdapter[U, V] extends EquationSystemTracer[U, V]:
  @elidable(TRACING)
  def beforeEvaluation(rho: Assignment[U, V], u: U): Unit = {}

  @elidable(TRACING)
  def afterEvaluation(rho: Assignment[U, V], u: U, res: V): Unit = {}

  @elidable(TRACING)
  def boxEvaluation(rho: Assignment[U, V], u: U, res: V, boxed: V): Unit = {}

object EquationSystemTracer:

  /**
    * An empty listener which does nothing.
    */
  private final class EmptyEquationSystemTracer[U, V] extends EquationSystemTracerAdapter[U, V]

  /**
    * A tracer which prints many debug informations on a PrintStream.
    */
  private final class DebugEquationSystemTracer[U, V](ps: PrintStream) extends EquationSystemTracer[U, V]:
    @elidable(TRACING)
    def beforeEvaluation(rho: Assignment[U, V], u: U) =
      ps.println(s"evaluated: $u oldvalue: ${rho(u)}")

    @elidable(TRACING)
    def afterEvaluation(rho: Assignment[U, V], u: U, res: V) =
      ps.println(s"evaluated: $u oldvalue: ${rho(u)} newvalue: $res")

    @elidable(TRACING)
    def boxEvaluation(rho: Assignment[U, V], u: U, res: V, boxed: V) =
      ps.println(s"evaluated: $u, oldvalue: ${rho(u)}, newvalue: $res, boxed: $boxed")

  private val emptyEquationSystemTracer = EmptyEquationSystemTracer[Any, Any]

  private val debugEquationSystemTracer = DebugEquationSystemTracer[Any, Any](System.out)

  def empty[U, V]: EquationSystemTracer[U, V] = emptyEquationSystemTracer.asInstanceOf[EmptyEquationSystemTracer[U, V]]

  def debug[U, V]: EquationSystemTracer[U, V] = debugEquationSystemTracer.asInstanceOf[DebugEquationSystemTracer[U, V]]

  def debug[U, V](ps: PrintStream): EquationSystemTracer[U, V] = DebugEquationSystemTracer[U, V](ps)
