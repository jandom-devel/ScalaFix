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

/** An assignment with an human-readable result for the `toString` method. */
abstract class StandardAssignment[-U, +V] extends Assignment[U, V]:
  /**
   * Returns an updated assignment which maps the unknown `u` to the value `v`.
   */
  def updated[V2 >: V](u: U, v: V2): Assignment[U, V2] = ConditionalAssignment(u, v, this)

/**
 * An assignment that always returns the same value.
 *
 * @param v
 *   the fixed value returned by the assignment.
 */
private final class ConstantAssignment[U, V](v: V) extends StandardAssignment[U, V]:

  override def apply(u: U): V = v

  override def toString: String = s"constant value $v"

/**
 * An assignment backed by a map.
 *
 * @param m
 *   the map
 */
private final class MapBasedAssignment[U, V](m: Map[U, V]) extends StandardAssignment[U, V]:

  export m.apply

  override def toString: String = m.mkString("[ ", ", ", " ]")

/**
 * An assignment backed by a function.
 *
 * @param f
 *   the function
 * @param str
 *   the optional output of the `toString` method. If `None`, the standard
 *   `toString` for functions is used.
 */
@FunctionalInterface
private final class FunctionBasedAssignment[U, V](f: U => V, str: Option[String])
    extends StandardAssignment[U, V]:

  export f.apply

  override def toString: String = str.getOrElse(f.toString)

/**
 * An assignment obtained from another assignment by changing the value of a
 * single unknown with a given value.
 *
 * @param special
 *   the unknown whose value we want to change.
 * @param value
 *   the new value of the unknown `special`.
 * @param otherwise
 *   the initial assignment.
 */
private final class ConditionalAssignment[U, V](
    special: U,
    value: V,
    otherwise: StandardAssignment[U, V]
) extends StandardAssignment[U, V]:

  override def apply(u: U): V = if u == special then value else otherwise(u)

  override def toString: String = s"[ $special -> $value ] over $otherwise"
