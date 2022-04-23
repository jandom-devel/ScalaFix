/**
 * Copyright 2019 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.scalafix.assignments

import it.unich.scalafix.*

import scala.collection.MapFactory
import scala.collection.mutable

/**
 * A mutable assignment is an assignment used internally by fixpoint solvers. It
 * keeps track of unknowns which are explicitly modified. While an hash table is
 * the most obvious implementation, custom mutable assignment may be developed.
 *
 * @tparam U
 *   type for unknowns
 * @tparam V
 *   type for values
 */
trait MutableAssignment[U, V] extends Assignment[U, V]:
  /**
   * An iterable of the unknowns which have been explicitly modified. Unknowns
   * which have an associated value due to some default initial assignment
   * should not be listed here.
   */
  def unknowns: Iterable[U]

  /**
   * Determines whether the unknown `u` has been explicitly modified. Unknowns
   * which have an associated value due to some default initial assignment
   * should be reported as undefined.
   */
  def isDefinedAt(u: U): Boolean

  /** Update this assignment. */
  def update(u: U, v: V): Unit

/**
 * Collection of private classes implementing mutabled assignments and
 * corresponding public factory methods.
 */
object MutableAssignment:

  /**
   * A mutable assignment backed by a mutable map and a plain assignment. The
   * latter is used for unknowns which have not been explicitly updated.
   *
   * @param rho
   *   the assignment used for the value of unknowns which have not been
   *   explicitly updated.
   * @param factory
   *   a factory for maps. Changed this parameter it is possible to choose which
   *   implementation of mutable maps we want to use.
   */
  private final class MapBasedMutableAssignment[U, V](
      rho: Assignment[U, V],
      factory: MapFactory[mutable.Map] = mutable.Map
  ) extends MutableAssignment[U, V]:
    private val m = factory.empty[U, V].withDefault(rho)
    export m.{apply, update, isDefinedAt, keys as unknowns}
    override def toString: String = s"${m.mkString("[ ", ", ", " ]")} over $rho"

  /**
   * Returns a mutable assignment backed by a mutable map and a plain
   * assignment. The latter is used for unknowns which have not been explicitly
   * updated.
   *
   * @param rho
   *   the assignment used for the value of unknowns which have not been
   *   explicitly updated.
   * @param factory
   *   a factory for maps. Changed this parameter it is possible to choose which
   *   implementation of mutable maps we want to use.
   */
  def apply[U, V](
      rho: Assignment[U, V],
      factory: MapFactory[mutable.Map] = mutable.Map
  ): MutableAssignment[U, V] =
    MapBasedMutableAssignment(rho, factory)
