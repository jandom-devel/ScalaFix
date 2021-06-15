/**
  * Copyright 2019, 2021 Gianluca Amato <gianluca.amato@unich.it>
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

/**
  * An assignment for an equation system is a map from unknowns to values.
  *
  * @tparam U type for unknowns
  * @tparam V type for values
  */
type Assignment[-U, +V] = U => V

object Assignment:

  /**
    * An assignment that always returns the same value.
    *
    * @param v the default value returned by the assignment
    */
  private final class ConstantAssignment[-U, +V](v: V) extends Assignment[U, V]:
    def apply(u: U): V = v
    override def toString: String = s"constant value $v"

  /**
    * An assignment which reproduces the value of a map. Although the map might be
    * directly used as an assignment, this class overrides the `toString` method to be
    * more usefult for debugging.
    *
    * @param m hash map
    */
  private final class MapBasedAssignment[-U, +V](m: Map[U, V]) extends Assignment[U, V]:
    export m.apply
    override def toString: String = m.mkString("[ ", ", ", " ]")

  /**
    * An assignment obtained from another assignment by modifying the value of an unknown with a given value.
    *
    * @param special    the unknown whose value we want to change
    * @param value      the new value of the unknown special
    * @param othwerwise the initial input assignment
    */
  private final class ConditionalAssignment[-U, +V](special: U, value: V, otherwise: Assignment[U, V]) extends Assignment[U, V]:
    def apply(u: U): V = if u == special then value else otherwise(u)
    override def toString: String = s"[ $special -> $value ] over $otherwise"

  def apply[U, V](f: U => V): Assignment[U,V] = f
  def apply[U, V](v: V): Assignment[U,V] = ConstantAssignment(v)
  def apply[U, V](m: Map[U, V]): Assignment[U,V] = MapBasedAssignment(m)

  def conditional[U, V](special: U, value: V, otherwise: Assignment[U, V]): Assignment[U,V] =
    ConditionalAssignment(special, value, otherwise)
