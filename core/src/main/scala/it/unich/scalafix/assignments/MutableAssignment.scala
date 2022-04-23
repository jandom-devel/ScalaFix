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
 * A mutable assignment is an assignment used for the inner working of fixpoint
 * solvers. It also explicitly keeps track of unknowns which are explicitly
 * modified.
 *
 * @tparam U
 *   type for unknowns
 * @tparam V
 *   type for values
 */
trait MutableAssignment[U, V] extends Assignment[U, V]:
  /** An iterable of unknowns which have been explicitly modified. */
  def unknowns: Iterable[U]

  /** Determines whether the unknown u has been explicitly modified. */
  def isDefinedAt(u: U): Boolean

  /** Update a mutable assignment. */
  def update(u: U, v: V): Unit

object MutableAssignment:

  /**
   * A mutable assignment which builds assignments based on mutable maps.
   *
   * @param a
   *   the assignment which is the original content of this IOAssignment
   */
  private final class MapBasedMutableAssignment[U, V](
      a: Assignment[U, V],
      factory: MapFactory[mutable.Map] = mutable.Map
  ) extends MutableAssignment[U, V]:
    private val m = factory.empty[U, V].withDefault(a)
    export m.{apply, update, isDefinedAt, keys as unknowns}
    override def toString: String = s"${m.mkString("[ ", ", ", " ]")} over $a}"

  def apply[U, V](
      a: Assignment[U, V],
      factory: MapFactory[mutable.Map] = mutable.Map
  ): MutableAssignment[U, V] =
    MapBasedMutableAssignment(a, factory)
