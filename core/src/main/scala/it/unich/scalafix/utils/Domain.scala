/**
 * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.scalafix.utils

/**
 * A `Domain` is a partial ordering with the additional contract that `equiv(x,
 * y)` iff `x == y`. This is probably not a brilliant idea, since it makes the
 * `Domain[V]` type-class too much dependent on the equality iin `V`. We will
 * revaluate this choice in a future version of ScalaFix.
 *
 * The trait also introduces a method for computing the upper bound. Not all
 * pairs of elements have an upper bound. Generally, elements in a domain are
 * partitioned in fibers, and an upper bound only exists for elements on the
 * same fiber. This is not modeled by the current definition of `Domain`.
 */
trait Domain[A] extends PartialOrdering[A]:

  /** Returns the upper bound of `x` and `y`. */
  def upperBound(x: A, y: A): A

  /** Extension methods for the type `A`. */
  extension (x: A)

    /** Uperrbound between `x` and `y`. */
    inline infix def \/(y: A): A = upperBound(x, y)

    inline infix def <=(y: A): Boolean = lteq(x, y)

    inline infix def <(y: A): Boolean = lt(x, y)

    inline infix def >=(y: A): Boolean = lteq(y, x)

    inline infix def >(y: A): Boolean = lt(y, x)

object Domain:
  /**
   * A domain obtained by a linear ordering, taking the max to be the upper
   * bound operator. Here we assume that if `lteq(x, y) == true` and `lteq(y, x)
   * \== true` then `x == y`, which is ensured by the contract of the
   * [[Ordering]] trait.
   */
  given orderingIsDomain[A](using o: Ordering[A]): Domain[A] with
    export o.{lteq, tryCompare}

    override def lt(x: A, y: A): Boolean = o.lt(x, y)

    override def gteq(x: A, y: A): Boolean = o.gteq(x, y)

    override def gt(x: A, y: A): Boolean = o.gt(x, y)

    override def equiv(x: A, y: A): Boolean = o.equiv(x, y)

    override def upperBound(x: A, y: A): A = if o.lt(x, y) then y else x
