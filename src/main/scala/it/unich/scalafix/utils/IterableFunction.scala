/**
  * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.utils

/**
  * An `IterableFunction` is a partial function with an iterator which returns the collection
  * of all its bindings. It is half way between a `PartialFunction` and a `Map`.
  *
  * @tparam A the domain of the function.
  * @tparam B the codomain of the function.
  */
trait IterableFunction[A, +B] extends PartialFunction[A, B] with Iterable[(A, B)] {
  /**
    * Returns the domain of the function.
    */
  def keys: Iterable[A]

  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    this.iterator.map { case (k, v) => k + " -> " + v }.addString(b, start, sep, end)

  override def stringPrefix: String = "IterableFunction"
}

object IterableFunction {

  private object EmptyIterableFunction extends IterableFunction[Any, Nothing] {
    def apply(x: Any) = throw new NoSuchElementException("argument not found: " + x)

    def isDefinedAt(x: Any) = false

    def iterator = Iterator.empty

    def keys = Iterable.empty
  }

  /**
    * An iterable function returned by an (eventually mutable) map.
    */
  implicit class MapIterableFunction[U, T](m: collection.Map[U, T]) extends IterableFunction[U, T] {
    def apply(x: U) = m(x)

    def isDefinedAt(x: U) = m.isDefinedAt(x)

    def iterator = m.iterator

    def keys = m.keys
  }

  /**
    * Returns an empty iterable function.
    */
  def empty[U, T]: IterableFunction[U, T] = EmptyIterableFunction.asInstanceOf[IterableFunction[U, T]]
}
