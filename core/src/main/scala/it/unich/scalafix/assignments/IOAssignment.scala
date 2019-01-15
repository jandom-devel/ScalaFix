/**
  * Copyright 2019 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.scalafix.assignments

import it.unich.scalafix.Assignment
import it.unich.scalafix.assignments.InputAssignment.hashToString

/**
  * An IOAssignment is an assignment used for input, output and internal working of fixpoint solvers. It also
  * explicitly keeps track of unknowns which are modified after creation time.
  *
  * @tparam U type for unknowns
  * @tparam V type for values
  */
trait IOAssignment[U, V] extends InputAssignment[U, V] {
  /**
    * An iterable of unknowns which have been explicitly modified after creation time.
    */
  def unknowns: Iterable[U]

  /**
    * Determines whether the unknown u has been explicitly modified after creation time.
    */
  def isDefinedAt(u: U): Boolean

  /**
    * Update an IOAssignment assignment.
    */
  def update(u: U, v: V): Unit
}

object IOAssignment {

  /**
    * An IOAssignment built using hash maps with a default argument.
    *
    * @param a the assignment which is the original content of this IOAssignment
    */
  class HashBasedIOAssignment[U, V](a: Assignment[U, V]) extends IOAssignment[U, V] {
    private val m = collection.mutable.HashMap.empty[U, V].withDefault(a)

    def apply(u: U): V = m(u)

    def update(u: U, v: V): Unit = m.update(u, v)

    def unknowns: Iterable[U] = m.keys

    def isDefinedAt(u: U): Boolean = m.isDefinedAt(u)

    def toIOAssignment: IOAssignment[U, V] = new HashBasedIOAssignment(this)

    override def toString: String = s"${hashToString(m)} over default $a"
  }

}
