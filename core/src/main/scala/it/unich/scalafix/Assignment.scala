/**
 * Copyright 2022 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.scalafix.assignments.*

/**
 * An assignment is just an alias for a functon from unknowns to values.
 *
 * @tparam U
 *   type for unknowns
 * @tparam V
 *   type for values
 */
type Assignment[-U, +V] = (U => V)

/**
 * A mutable assignment is an asisgnmetn used internally and retuned by fixpoint
 * solvers. Every mutable assignment is built over an initial plain assignment,
 * and keeps track of the unknowns which are explicitly modified after creation.
 *
 * @tparam U
 *   type for unknowns
 * @tparam V
 *   type for values
 */
trait MutableAssignment[U, V] extends Assignment[U, V]:
  /**
   * An iterable of the unknowns which have been explicitly modified w.r.t. the
   * initial assignment.
   */
  def unknowns: Iterable[U]

  /**
   * Determines whether the unknown `u` has been explicitly modified w.r.t. the
   * initial assignment.
   */
  def isDefinedAt(u: U): Boolean

  /** Update this assignment. */
  def update(u: U, v: V): Unit

/**
 * Collection of factory methods for generating assignments with a
 * human-readable `toString` method.
 */
object Assignment:

  /**
   * Returns a constant assignment mapping all unknowns to the same value `v`.
   *
   * @param v
   *   the fixed value returned by the assignment.
   */
  def apply[U, V](v: V): StandardAssignment[U, V] = ConstantAssignment(v)

  /**
   * Returns an assignment backed by an immutable map.
   *
   * @param m
   *   the immutable map holding the assignment.
   */
  def apply[U, V](m: Map[U, V]): StandardAssignment[U, V] = MapBasedAssignment(m)

  /**
   * Returns an assignment backed by a function
   *
   * @param f
   *   the function holding the assignment.
   * @param str
   *   the optional output of the `toString` method. If `None`, the standard
   *   `toString` for functions is used.
   */
  def apply[U, V](f: U => V, str: Option[String]): StandardAssignment[U, V] =
    FunctionBasedAssignment(f, str)
