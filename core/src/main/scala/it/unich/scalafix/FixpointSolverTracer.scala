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

import java.io.PrintStream

import scala.annotation.elidable

/**
  * A FixpointSolverTracer implements some methods which are called by solvers when certain
  * events occurs. They may be used for debugging, tracing, etc...
  *
  * @tparam U the type of unknowns supported by this tracer
  * @tparam V the type of values for unknowns supported by this tracer
  */
trait FixpointSolverTracer[U, V] {
  /**
    * This method is called when a fixpoint solver is started with the initial assignment.
    *
    * @param rho the current assignment
    */
  @elidable(TRACING)
  def initialized(rho: Assignment[U, V])

  /**
    * This method is called when the final assignment has been computed.
    *
    * @param rho the current assignment
    */
  @elidable(TRACING)
  def completed(rho: Assignment[U, V])

  /**
    * This method is called when an unknown `u` is evaluated.
    *
    * @param rho    the current assignment
    * @param u      the unknown which is evaluated
    * @param newval the result of the evaluation
    */
  @elidable(TRACING)
  def evaluated(rho: Assignment[U, V], u: U, newval: V)

  /**
    * This is called when the ascending phase begins in a two phase solver.
    *
    * @param rho the assignment at the beginning of the ascending phase
    */
  @elidable(TRACING)
  def ascendingBegins(rho: Assignment[U, V])

  /**
    * This is called when the descending phase begins in a two phase solver.
    *
    * @param rho the assignment at the beginning of the descending phase
    */
  @elidable(TRACING)
  def descendingBegins(rho: Assignment[U, V])
}

/**
  * This abstract class implements a tracer for fixpoint solvers which does nothing.
  * May be sub-classed in order to override only the methods we are interested in.
  */
abstract class FixpointSolverTracerAdapter[U, V] extends FixpointSolverTracer[U, V] {
  def evaluated(rho: Assignment[U, V], u: U, newval: V) {}

  def initialized(rho: Assignment[U, V]) {}

  def completed(rho: Assignment[U, V]) {}

  def ascendingBegins(rho: Assignment[U, V]) {}

  def descendingBegins(rho: Assignment[U, V]) {}
}

object FixpointSolverTracer {

  /**
    * An empty tracer which does nothing.
    */
  class EmptyFixpointSolverTracer[U, V] extends FixpointSolverTracerAdapter[U, V]

  /**
    * A tracer which prints all the informations on a PrintStream.
    */
  class DebugFixpointSolverTracer[U, V](ps: PrintStream) extends FixpointSolverTracer[U, V] {
    def evaluated(rho: Assignment[U, V], u: U, newval: V) {
      ps.println(s"evaluated: $u oldvalue: ${rho(u)} newvalue: $newval")
    }

    def completed(rho: Assignment[U, V]) {
      ps.println(s"completed with assignment $rho")
    }

    def initialized(rho: Assignment[U, V]) {
      ps.println(s"initialized with assignment $rho")
    }

    def ascendingBegins(rho: Assignment[U, V]) {
      ps.println(s"ascending chain begins with assignment $rho")
    }

    def descendingBegins(rho: Assignment[U, V]) {
      ps.println(s"descending chain begins with assignment $rho")
    }
  }

  /**
    * A tracer which keeps track of performance measures.
    */
  class PerformanceFixpointSolverTracer[U, V] extends FixpointSolverTracerAdapter[U, V] {

    private var numeval: Int = 0

    /**
      * Number of evaluations of r.h.s. performed so far.
      */
    def evaluations: Int = numeval

    override def evaluated(rho: Assignment[U, V], u: U, newval: V) {
      numeval += 1
    }
  }

  private val emptyFixpointSolverTracer = new EmptyFixpointSolverTracer[Any, Any]

  private val debugFixpointSolverTracer = new DebugFixpointSolverTracer[Any, Any](System.out)

  private val performanceFixpointSolverTracer = new PerformanceFixpointSolverTracer[Any, Any]

  /**
    * Returns a fixpoint solver tracer which does nothing.
    */
  def empty[U, V]: EmptyFixpointSolverTracer[U, V] = emptyFixpointSolverTracer.asInstanceOf[EmptyFixpointSolverTracer[U, V]]

  /**
    * Returns a debug fixpoint solver tracer which prints debugging information to standard output.
    */
  def debug[U, V]: DebugFixpointSolverTracer[U, V] = debugFixpointSolverTracer.asInstanceOf[DebugFixpointSolverTracer[U, V]]

  /**
    * Returns a debug fixpoint solver tracer which prints debugging information to the specified PrintStream.
    */
  def debug[U, V](ps: PrintStream) = new DebugFixpointSolverTracer[U, V](ps)

  /**
    * Returns a performance fixpoint solver tracer.
    */
  def performance[U, V]: PerformanceFixpointSolverTracer[U, V] = performanceFixpointSolverTracer.asInstanceOf[PerformanceFixpointSolverTracer[U, V]]
}