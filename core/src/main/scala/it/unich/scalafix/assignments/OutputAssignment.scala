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

package it.unich.scalafix.assignments

import it.unich.scalafix.Assignment

import scala.collection.mutable

/**
 * An assignment returned as an output from a fixpoint solver. It is an
 * extension of an input assignment which also returns the list of unknowns
 * which have been modified w.r.t. the initial assignment given as input to the
 * solver.
 */
trait OutputAssignment[U, V] extends InputAssignment[U, V]:

  /** An iterable of the unknowns which have been explicitly modified. */
  def unknowns: Iterable[U]

  /** Determines whether the unknown `u` has been explicitly modified. */
  def isDefinedAt(u: U): Boolean

  /**
   * Returns a mutable assignment whose initial value is the same as this. If an
   * unknown is considered to be explicitly modified here, then it is considered
   * explicitly modified even in the resulting mutable assignment.
   */
  override def toMutableAssignment: MutableAssignment[U, V]

/**
 * An output assignment backed by a map `m` with initial input assignment `rho`.
 */
private[assignments] final class MapBasedOutputAssignment[U, V](
    m: mutable.Map[U, V],
    rho: InputAssignment[U, V]
) extends OutputAssignment[U, V]:

  export m.{apply, isDefinedAt, keys as unknowns}

  override def toMutableAssignment = MutableAssignment(this, m)

  override def toString: String = s"${m.mkString("[ ", ", ", " ]")} over $rho"
