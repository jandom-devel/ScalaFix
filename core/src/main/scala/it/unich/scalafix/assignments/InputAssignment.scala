/**
 * Copyright 2019, 2021, 2022 Gianluca Amato <gianluca.amato@unich.it>
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

/**
 * An assignment which is used as the input of a fixpoint solver. It has a
 * method for obtaining a mutable assignment out of it. Moreover, an input
 * assignment should provide a `toString()` method which returns human-readable
 * information.
 */
trait InputAssignment[U, V] extends Assignment[U, V]:
  /** Returns a mutable assignment whose initial value is the same as this. */
  def toMutableAssignment: MutableAssignment[U, V]

/**
 * Base class for all built-in implementations of the Input assignments trait.
 */
sealed abstract class StandardInputAssignment[U, V] extends InputAssignment[U, V]:
  /**
   * Returns an updated assignment which maps the unknown `u` to the value `v`.
   */
  def updated(u: U, v: V): InputAssignment[U, V] = ConditionalInputAssignment(u, v, this)

  override def toMutableAssignment = MutableAssignment(this)

/**
 * An input assignment that always returns the same value.
 *
 * @param v
 *   the fixed value returned by the assignment
 */
private[assignments] final class ConstantInputAssignment[U, V](v: V)
    extends StandardInputAssignment[U, V]:

  override def apply(u: U): V = v

  override def toString: String = s"constant value $v"


/**
 * An input assignment backed by a map.
 *
 * @param m
 *   the map
 */
private[assignments] final class MapBasedInputAssignment[U, V](m: Map[U, V])
    extends StandardInputAssignment[U, V]:

  export m.apply

  override def toString: String = m.mkString("[ ", ", ", " ]")

/**
 * An input assignment obtained from another input assignment by changing the
 * value of a single unknown with a given value.
 *
 * @param special
 *   the unknown whose value we want to change
 * @param value
 *   the new value of the unknown `special`
 * @param otherwise
 *   the initial Input assignment
 */
private[assignments] final class ConditionalInputAssignment[U, V](
    special: U,
    value: V,
    otherwise: StandardInputAssignment[U, V]
) extends StandardInputAssignment[U, V]:

  override def apply(u: U): V = if u == special then value else otherwise(u)

  override def toString: String = s"[ $special -> $value ] over $otherwise"

/** Collection of factory methods for standard input assignments. */
object InputAssignment:

  /**
   * Returns a constant assignment mapping all unknowns to the same value `v`.
   *
   * @param v
   *   the fixed value returned by the assignment
   */
  def apply[U, V](v: V): StandardInputAssignment[U, V] = ConstantInputAssignment(v)

  /**
   * Returns an assignment backed by a map `m`. Although the map might be
   * directly used as an assignment, this class overrides the `toString()`
   * method in order to display its content.
   *
   * @param m
   *   the map
   */
  def apply[U, V](m: Map[U, V]): StandardInputAssignment[U, V] = MapBasedInputAssignment(m)
