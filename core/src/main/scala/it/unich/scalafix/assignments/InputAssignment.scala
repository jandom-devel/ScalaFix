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

/**
  * An input assignment is an assignment used as the initial assignment for fixpoint solvers. It has a method
  * which converts the assignment into a mutable assignment. Actual implementations of InputAssignment should provide
  * a toString method which gives detailed information, to be used for tracing and debugging purposes.
  *
  * @tparam U type for unknowns
  * @tparam V type for values
  */
trait InputAssignment[U, V] extends Assignment[U, V] {
  /**
    * Returns an IOAssignment with the same initial content as the current assignment. Changes to the IOAssignment
    * are not reflected to the current assignment.
    */
  def toIOAssignment: IOAssignment[U, V]
}

/**
  * The companion object for the class `it.unich.scalafix.assignments.InputAssignment`.
  */
object InputAssignment {

  protected[assignments] def hashToString[U, V](m: collection.Map[U, V]): String = {
    val sb = new StringBuffer()
    var first: Boolean = true
    sb.append(" [ ")
    for ((u, v) <- m) {
      if (!first) sb.append(" , ")
      first = false
      sb.append(u).append(" -> ").append(v)
    }
    sb.append(" ]")
    sb.toString
  }

  /**
    * An assignment that always returns the same default value.
    *
    * @param v the default value returned by the assignment
    */
  implicit class Default[U, V](v: V) extends InputAssignment[U, V] {
    def apply(u: U): V = v

    def toIOAssignment: IOAssignment[U, V] = new IOAssignment.HashBasedIOAssignment({ _: U => v })

    override def toString: String = s"constant value $v"
  }

  /**
    * An input assignment obtained from another input assignment by modifying the value of an unknown with
    * a given binding.
    *
    * @param special the unknown whose value we want to change
    * @param value   the new value of the unknown special
    * @param default the initial input assignment
    */
  class Conditional[U, V](special: U, value: V, default: InputAssignment[U, V]) extends InputAssignment[U, V] {
    def apply(u: U): V = if (u == special) value else default(u)

    def toIOAssignment: IOAssignment[U, V] = new IOAssignment.HashBasedIOAssignment(this)

    override def toString: String = s"[ $special -> $value ] else $default"
  }

  /**
    * An implicit class converting a map into an input assignment.
    *
    * @param m hash map
    */
  implicit class FromMap[U, V](m: Map[U, V]) extends InputAssignment[U, V] {
    def apply(u: U): V = m(u)

    def toIOAssignment: IOAssignment[U, V] = new IOAssignment.HashBasedIOAssignment(this)

    override def toString: String = hashToString(m)
  }

  /**
    * An implicit class converting an assignment into an input assignment.
    *
    * @param a the original assignment
    */
  implicit class FromAssignment[U, V](a: Assignment[U, V]) extends InputAssignment[U, V] {
    def apply(u: U): V = a(u)

    def toIOAssignment: IOAssignment[U, V] = new IOAssignment.HashBasedIOAssignment(a)

    override def toString: String = a.toString
  }

  /**
    * Return the input assignment which behaves like `otherwise`, with the excpetion of the unknown
    * `special` for which it returns `value`.
    */
  def conditional[U, V](special: U, value: V, otherwise: InputAssignment[U, V]) = new Conditional(special, value, otherwise)
}
