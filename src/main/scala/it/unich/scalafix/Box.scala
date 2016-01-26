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

package it.unich.scalafix

import it.unich.scalafix.lattice.{Domain, Magma}

import scala.language.implicitConversions

/**
  * A Box is a way to combine two values into a new one. It is a specialization of the functional type
  * `(V,V) => V`, where the first parameter is the old value of an unknown and the second parameter
  * is the new contribution. Both widenings and narrowings are examples of boxes. Boxes are mutable, i.e.,
  * the apply method may give different results for the same input when called multiple times.
  *
  * Another function of boxes is to be blueprints for building other equivalent boxes. Each box has a copy
  * method which should produce a functionally equivalent copy of `this`. The copy method should try to
  * minimize object duplication.
  *
  * @tparam V the type of the values to combine.
  */
abstract class Box[V] extends Function2[V, V, V] {
  /*
  I do not like very much the fact Box is both a box and its blueprint. Having two separate classes would
  be better from the point of view of the separation of concerns, but this solution is quite convenient. We do
  not to write redundant code, we only need a copy method to properly and efficiently handling mutable objects,
  the API is simple. Keep in mind that one of the important point of this design is to reduce duplication of boxes
  in box assignments as much as possible.
   */

  /**
    * It returns true if the box is guaranteed to be idempotent, i.e., if `x box y = (x box y) box y`.
    * This may be used for optimization purposes.
    */
  def isIdempotent: Boolean

  /**
    * It returns true if this is guaranteed to be the right box (i.e., the one which always returns the second
    * component). This may be used for optimization purposes.
    */
  def isRight: Boolean

  /**
    * It returns true if this box is immutable, i.e., if the `apply` method does not change its behaviour
    * over time.
    */
  def isImmutable: Boolean

  /**
    * Returns a copy of this box. An immutable box may just returns itself, but a mutable ones should produce a
    * a copy of itself.
    */
  def copy: Box[V]

  /**
    * Returns a delayed version of the box. It is equivalent to applying cascade to the right box.
    */
  def delayed(delay: Int): Box[V] = Box.cascade(Box.right[V], delay, this)
}

/**
  * The `Box` object defines several factories for building boxes.
  */
// TODO: Decide whether we want to keep the variants for PartiallyOrdered.
object Box {

  abstract class ImmutableBox[V] extends Box[V] {
    def isImmutable = true

    def copy = this
  }

  private abstract class MutableBox[V] extends Box[V] {
    def isImmutable = false
  }

  private object RightBox extends ImmutableBox[Any] {
    def apply(x: Any, y: Any): Any = y

    def isIdempotent = true

    def isRight = true
  }

  private object LeftBox extends ImmutableBox[Any] {
    def apply(x: Any, y: Any): Any = x

    def isIdempotent = true

    def isRight = false
  }

  private final class FromFunction[V](f: (V, V) => V, val isIdempotent: Boolean) extends ImmutableBox[V] {
    def apply(x: V, y: V) = f(x, y)

    def isRight = false
  }

  private final class Warrowing[V](lteq: (V, V) => Boolean, widening: Box[V], narrowing: Box[V]) extends Box[V] {
    def apply(x: V, y: V) = if (lteq(y, x)) narrowing(x, y) else widening(x, y)

    def isIdempotent = false

    def isRight = false

    def isImmutable = widening.isImmutable && narrowing.isImmutable

    def copy = if (isImmutable) this else new Warrowing(lteq, widening.copy, narrowing.copy)
  }

  private final class Cascade[V](first: Box[V], delay: Int, second: Box[V]) extends MutableBox[V] {
    var steps = 0

    def isIdempotent = false

    def isRight = false

    def copy = new Cascade(first.copy, delay, second.copy)

    def apply(x: V, y: V): V = {
      if (steps < delay) {
        steps += 1
        first(x, y)
      } else
        second(x, y)
    }
  }

  /**
    * A box which always returns its right component (new contribution)
    */
  def right[V]: ImmutableBox[V] = RightBox.asInstanceOf[ImmutableBox[V]]

  /**
    * A box which always returns its left component (original value).
    */
  def left[V]: ImmutableBox[V] = LeftBox.asInstanceOf[ImmutableBox[V]]


  /**
    * A box built from a function `f: (V,V) => V`. The box is declared to be idempotent and immutable.
    *
    * @param f the function to use for the `apply` method of the new box.
    */
  implicit def apply[V](f: (V, V) => V): ImmutableBox[V] =
    new FromFunction(f, true)

  /**
    * A box built from a function `f: (V,V) => V`. The box is declared to be immutable, while idempotency
    * depends from the parameter `areIdempotent`
    *
    * @param f            the function to use for the `apply` method of the new box.
    * @param isIdempotent determines whether the returned box is declared to be idempotent
    */
  def apply[V](f: (V, V) => V, isIdempotent: Boolean): Box[V] =
    new FromFunction(f, isIdempotent)

  /**
    * A box given by the upper bound of a type `V` endowed with a directed partial ordering.
    */
  def upperBound[V: Domain]: ImmutableBox[V] =
    new FromFunction(Domain[V].upperBound, true)

  /**
    * A box given by the magma operator on a type `V`.
    */
  def magma[V: Magma]: ImmutableBox[V] =
    new FromFunction(Magma[V].op, true)

  /**
    * A mutable box which behaves as `this` for the first `delay` steps and as `that` for the rest of its
    * existence. This may be used to implement delayed widenings and narrowings.
    */
  def cascade[V](first: Box[V], delay: Int, second: Box[V]): Box[V] = {
    require(delay >= 0)
    if (first.isRight && second.isRight)
      Box.right[V]
    else if (delay == 0)
      second
    else
      new Cascade(first, delay, second)
  }

  /**
    * A warrowing, obtained combining the given widenings and narrowings, as defined in the paper:
    * Amato, Scozzari, Seidl, Apinis, Vodjani
    * "Efficiently intertwining widenings and narrowings".
    * Science of Computer Programming
    *
    * @tparam V the type of values, should be partially ordered
    * @param widening  is a widenings over V
    * @param narrowing is a narrowings over V
    */
  def warrowing[V <: PartiallyOrdered[V]](widening: Box[V], narrowing: Box[V]): Box[V] = {
    if (widening.isRight && narrowing.isRight)
      right[V]
    else
      new Box[V] {
        def apply(x: V, y: V) = if (y <= x) narrowing(x, y) else widening(x, y)

        def isIdempotent = false

        def isRight = false

        def isImmutable = widening.isImmutable && narrowing.isImmutable

        def copy = if (isImmutable) this else warrowing(widening.copy, narrowing.copy)
      }
  }

  /**
    * A warrowing, obtaiend combining the given widenings and narrowings, as defined in the paper:
    * Amato, Scozzari, Seidl, Apinis, Vodjani
    * "Efficiently intertwining widenings and narrowings".
    * Science of Computer Programming
    *
    * @tparam V the type of values, should be endowed with a partial ordering
    * @param widening  is widenings over V
    * @param narrowing is a narrowings over V
    */
  def warrowing[V: PartialOrdering](widening: Box[V], narrowing: Box[V]): Box[V] = {
    if (widening.isRight && narrowing.isRight)
      right[V]
    else
      new Warrowing(implicitly[PartialOrdering[V]].lteq, widening, narrowing)
  }

}
