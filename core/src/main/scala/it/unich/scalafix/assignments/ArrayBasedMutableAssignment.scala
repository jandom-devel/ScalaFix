 /**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it>
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
import it.unich.scalafix.MutableAssignment

import scala.reflect.ClassTag

/**
 * A mutable assignment backed by an array.
 *
 * @param rho
 *   the initial value of this assignment.
 * @param size
 *   the size of the array. This assignment may only be used for unknowns in the
 *   range from `0` to `size-1`.
 */
class ArrayBasedMutableAssignment[V: ClassTag](
    rho: Assignment[Int, V],
    size: Int
) extends MutableAssignment[Int, V]:

  private val a = new Array[V](size)
  private val changed = Array.fill(size)(false)

  /** @inheritdoc */
  override def update(i: Int, v: V) =
    a.update(i, v)
    changed.update(i, true)

  /** Returns the value assigned to the unknown `i`. */
  override def apply(i: Int) =
    a(i)
    if changed(i) then a(i) else rho(i)

  /** @inheritdoc */
  override def isDefinedAt(i: Int) = changed(i)

  /** @inheritdoc */
  override def unknowns = (0 until size).filter(changed(_))

  /** @inheritdoc */
  override def iterator = (0 until size).view.map( x => (x, this(x)) ).iterator

  /**
   * @inheritdoc
   *
   * The string representation is human-readable, and shows both the updated
   * bindings of the assignment and the initial assignment.
   */
  override def toString: String = s"${a.mkString("[ ", ", ", " ]")} over $rho"
