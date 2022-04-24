/**
 * Copyright 2015, 2016, 2021 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.scalafix

import it.unich.scalafix.lattice.{Domain, Magma}

/**
 * A Combo is a way to combine two values into a new one. It is a specialization
 * of the functional type `(V,V) => V`, where the first parameter is the old
 * value of an unknown and the second parameter is the new contribution. Both
 * widenings and narrowings are examples of combos. Combos are mutable, i.e., the
 * apply method may give different results for the same input when called
 * multiple times.
 *
 * Another function of combos is to be blueprints for building other equivalent
 * combos. Each combo has a copy method which should produce a functionally
 * equivalent copy of `this`. The copy method should try to minimize object
 * duplication.
 *
 * @tparam V
 *   the type of the values to combine.
 */
abstract class Combo[V] extends ((V, V) => V):
  /*
  I do not like very much the fact Combo is both a combo and its blueprint. Having two separate classes would
  be better from the point of view of the separation of concerns, but this solution is quite convenient. We do
  not write redundant code, we only need a copy method to properly and efficiently handle mutable objects,
  the API is simple. Keep in mind that one of the important point of this design is to reduce duplication of combos
  in combo assignments as much as possible.
   */

  /**
   * It returns true if the combo is guaranteed to be idempotent, i.e., if `x combo
   * y = (x combo y) combo y`. This may be used for optimization purposes.
   */
  def isIdempotent: Boolean

  /**
   * It returns true if this is guaranteed to be the right combo (i.e., the one
   * which always returns the second component). This may be used for
   * optimization purposes.
   */
  def isRight: Boolean

  /**
   * It returns true if this combo is guaranteed to be immutable, i.e., if the
   * `apply` method does not change its behaviour over time.
   */
  def isImmutable: Boolean

  /**
   * Returns a copy of this combo. An immutable combo may just returns itself, but a
   * mutable one should produce a distinct copy of itself.
   */
  def copy: Combo[V]

  /**
   * Returns a delayed version of the combo. It is equivalent to applying cascade
   * to the right combo.
   */
  def delayed(delay: Int): Combo[V] = Combo.cascade(Combo.right[V], delay, this)

/** The `Combo` object defines several factories for building combos. */
object Combo:

  abstract class ImmutableCombo[V] extends Combo[V]:
    def isImmutable = true
    def copy: this.type = this

  private object RightCombo extends ImmutableCombo[Any]:
    def apply(x: Any, y: Any): Any = y
    def isRight = true
    def isIdempotent = true

  private object LeftCombo extends ImmutableCombo[Any]:
    def apply(x: Any, y: Any): Any = x
    def isRight = false
    def isIdempotent = true

  // We assume f is immutable, since we would not know how to handle the case with f mutable.
  private final class FromFunction[V](f: (V, V) => V, val isIdempotent: Boolean)
      extends ImmutableCombo[V]:
    def apply(x: V, y: V): V = f(x, y)
    def isRight = false

  // we only consider the case when either `first` or `second` is not a right combo
  private final class Warrowing[V: PartialOrdering](widening: Combo[V], narrowing: Combo[V])
      extends Combo[V]:
    def apply(x: V, y: V): V =
      if summon[PartialOrdering[V]].lteq(y, x) then narrowing(x, y) else widening(x, y)
    def isRight: Boolean = false
    def isIdempotent: Boolean = false
    def isImmutable: Boolean = widening.isImmutable && narrowing.isImmutable
    def copy: Warrowing[V] = if isImmutable then this else Warrowing(widening.copy, narrowing.copy)

  // we only consider the case when `delay` > 0 and either `first` or `second` is not a right combo
  private final class Cascade[V](first: Combo[V], delay: Int, second: Combo[V]) extends Combo[V]:
    var steps = 0

    def isRight = false
    def isIdempotent = false
    def isImmutable = false
    def copy = Cascade(first.copy, delay, second.copy)

    def apply(x: V, y: V): V =
      if steps < delay then
        steps += 1
        first(x, y)
      else second(x, y)

  /** A combo which always returns its right component (new contribution). */
  def right[V]: ImmutableCombo[V] = RightCombo.asInstanceOf[ImmutableCombo[V]]

  /** A combo which always returns its left component (original value). */
  def left[V]: ImmutableCombo[V] = LeftCombo.asInstanceOf[ImmutableCombo[V]]

  /**
   * A combo built from a function `f: (V,V) => V`. The combo is declared to be
   * immutable, while idempotency depends from the parameter `areIdempotent`
   *
   * @param f
   *   the function to use for the `apply` method of the new combo.
   * @param isIdempotent
   *   determines whether the returned combo is declared to be idempotent (default
   *   is `true`)
   */
  def apply[V](f: (V, V) => V, isIdempotent: Boolean = true): Combo[V] = FromFunction(f, isIdempotent)

  /**
   * A combo given by the upper bound of a type `V` endowed with a directed
   * partial ordering.
   */
  def upperBound[V: Domain]: ImmutableCombo[V] = FromFunction(_ upperBound _, true)

  /** A combo given by the magma operator on a type `V`. */
  def magma[V: Magma]: ImmutableCombo[V] = FromFunction(_ op _, true)

  /**
   * A mutable combo which behaves as `this` for the first `delay` steps and as
   * `that` for the rest of its existence. This may be used to implement delayed
   * widenings and narrowings.
   */
  def cascade[V](first: Combo[V], delay: Int, second: Combo[V]): Combo[V] =
    require(delay >= 0)
    if first.isRight && second.isRight then Combo.right[V]
    else if delay == 0 then second
    else Cascade(first, delay, second)

  /**
   * A warrowing obtained by combining the given widenings and narrowings, as
   * defined in the paper: Amato, Scozzari, Seidl, Apinis, Vodjani "Efficiently
   * intertwining widenings and narrowings". Science of Computer Programming
   *
   * @tparam V
   *   the type of values, should be endowed with a partial ordering
   * @param widening
   *   a widening over V
   * @param narrowing
   *   a narrowing over V
   */
  def warrowing[V: PartialOrdering](widening: Combo[V], narrowing: Combo[V]): Combo[V] =
    if widening.isRight && narrowing.isRight then right[V]
    else Warrowing(widening, narrowing)
