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

package it.unich.scalafix.lattice

/**
 * A `Domain` is a `PartialOrdering` where elements are endowed with an upper
 * bound operator. However, not all pairs of elements have an upper bound.
 * Generally, elements in a domain are partitioned in fibers, and an upper bound
 * only exists for elements on the same fiber. This is not modeled by the
 * current definition of `Domain`.
 */
trait Domain[A] extends PartialOrdering[A]:

  extension (x: A)
    /** It returns an upper bound of `x` and `y`. */
    infix def upperBound(y: A): A

/**
 * A domain obtained by an ordering, taking the max to be the upper bound
 * operator.
 */
given orderingIsDomain[A](using o: Ordering[A]): Domain[A] with
  def lteq(x: A, y: A): Boolean = o.lteq(x, y)

  def tryCompare(x: A, y: A): Some[Int] = o.tryCompare(x, y)

  extension (x: A) def upperBound(y: A): A = if o.compare(x, y) <= 0 then y else x
