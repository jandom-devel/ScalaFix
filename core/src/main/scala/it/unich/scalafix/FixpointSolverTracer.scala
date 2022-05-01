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

package it.unich.scalafix

import java.io.PrintStream

import scala.annotation.elidable

/**
 * Implements some methods which are called by solvers when certain events
 * occurs. They may be used for debugging, tracing, etc...
 *
 * @tparam U
 *   the type of unknowns supported by this tracer.
 * @tparam V
 *   the type of values for unknowns supported by this tracer.
 */
trait FixpointSolverTracer[U, V]:
  /**
   * This method is called when a fixpoint solver is started with the initial
   * assignment.
   *
   * @param rho
   *   the initial assignment.
   */
  def initialized(rho: Assignment[U, V]): Unit

  /**
   * This method is called when the final assignment has been computed.
   *
   * @param rho
   *   the final assignment.
   */
  def completed(rho: Assignment[U, V]): Unit

  /**
   * This method is called when an unknown `u` is evaluated.
   *
   * @param rho
   *   the current assignment.
   * @param u
   *   the unknown which is evaluated.
   * @param newval
   *   the result of the evaluation.
   */
  def evaluated(rho: Assignment[U, V], u: U, newval: V): Unit

  /**
   * This is called when the ascending phase begins in a two phase solver.
   *
   * @param rho
   *   the assignment at the beginning of the ascending phase.
   */
  def ascendingBegins(rho: Assignment[U, V]): Unit

  /**
   * This is called when the descending phase begins in a two phase solver.
   *
   * @param rho
   *   the assignment at the beginning of the descending phase.
   */
  def descendingBegins(rho: Assignment[U, V]): Unit

/**
 * This abstract class implements a tracer for fixpoint solvers which does
 * nothing. May be sub-classed in order to override only the methods we are
 * interested in.
 */
abstract class FixpointSolverTracerAdapter[U, V] extends FixpointSolverTracer[U, V]:
  /** It does nothing. */
  @elidable(TRACING)
  override def evaluated(rho: Assignment[U, V], u: U, newval: V) = {}

  /** It does nothing. */
  @elidable(TRACING)
  override def initialized(rho: Assignment[U, V]) = {}

  /** It does nothing. */
  @elidable(TRACING)
  override def completed(rho: Assignment[U, V]) = {}

  /** It does nothing. */
  @elidable(TRACING)
  override def ascendingBegins(rho: Assignment[U, V]) = {}

  /** It does nothing. */
  @elidable(TRACING)
  override def descendingBegins(rho: Assignment[U, V]) = {}

/** A tracer which keeps track of performance measures. */
class PerformanceFixpointSolverTracer[U, V] extends FixpointSolverTracerAdapter[U, V]:

  private var numeval: Int = 0

  /** Number of evaluations of body performed so far. */
  def evaluations: Int = numeval

  /**
   * Increments the number of performed evaluations returned by the
   * `evaluations` method.
   */
  @elidable(TRACING)
  override def evaluated(rho: Assignment[U, V], u: U, newval: V) =
    numeval += 1

/** Collection of factory methods for fixpoint solver tracers. */
object FixpointSolverTracer {

  /** An empty tracer which does nothing. */
  private final class EmptyFixpointSolverTracer[U, V] extends FixpointSolverTracerAdapter[U, V]

  /** A tracer which prints many debug information to a PrintStream. */
  private final class DebugFixpointSolverTracer[U, V](ps: PrintStream)
      extends FixpointSolverTracer[U, V]:
    @elidable(TRACING)
    override def evaluated(rho: Assignment[U, V], u: U, newval: V) =
      ps.println(s"evaluated: $u oldvalue: ${rho(u)} newvalue: $newval")

    @elidable(TRACING)
    override def completed(rho: Assignment[U, V]) =
      ps.println(s"completed with assignment $rho")

    @elidable(TRACING)
    override def initialized(rho: Assignment[U, V]) =
      ps.println(s"initialized with assignment $rho")

    @elidable(TRACING)
    override def ascendingBegins(rho: Assignment[U, V]) =
      ps.println(s"ascending chain begins with assignment $rho")

    @elidable(TRACING)
    override def descendingBegins(rho: Assignment[U, V]) =
      ps.println(s"descending chain begins with assignment $rho")

  /** Instance of an tracer which does nothing. */
  private val emptyFixpointSolverTracer = new EmptyFixpointSolverTracer[Any, Any]

  /**
   * Instance of tracer which prints many debug information to standard output.
   */
  private val debugFixpointSolverTracer = new DebugFixpointSolverTracer[Any, Any](System.out)

  /** Returns a tracer which does nothing. */
  def empty[U, V]: FixpointSolverTracer[U, V] =
    emptyFixpointSolverTracer.asInstanceOf[EmptyFixpointSolverTracer[U, V]]

  /** Returns a tracer which prints many information to standard output. */
  def debug[U, V]: FixpointSolverTracer[U, V] =
    debugFixpointSolverTracer.asInstanceOf[DebugFixpointSolverTracer[U, V]]

  /**
   * Returns a tracer which prints debug debugging information to a PrintStream.
   */
  def debug[U, V](ps: PrintStream): FixpointSolverTracer[U, V] =
    DebugFixpointSolverTracer(ps)

  /** Returns a tracer which keeps track of performance measures. */
  def performance[U, V] = PerformanceFixpointSolverTracer()
}
