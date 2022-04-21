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
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.lattice

import scala.language.implicitConversions

/**
  * A `Domain` is a `PartialOrdering` where elements are endowed with an upper bound operator. However, not all
  * pairs of elements have an upper bound. Generally, elements in a domain are partitioned in fibers, and an
  * upper bound only exists for elements on the same fiber. This is not modeled by the current definition of `Domain`.
  *
  * If an implicit object of type `Domain[A]` is in scope, then binary operators
  * `<`, `<=`, `>`, `>=`, `equiv` and `upperBound` are available.
  */
trait Domain[A] extends PartialOrdering[A]:
  /**
    * It returns an upper bound of `x` and  `y`.
    */
  def upperBound(x: A, y: A): A

  class Ops(lhs: A):
    def <(rhs: A): Boolean = lt(lhs, rhs)

    def <=(rhs: A): Boolean = lteq(lhs, rhs)

    def >=(rhs: A): Boolean = gteq(lhs, rhs)

    def >(rhs: A): Boolean = gt(lhs, rhs)

    def equiv(rhs: A): Boolean = Domain.this.equiv(lhs, rhs)

    def upperBound(rhs: A): A = Domain.this.upperBound(lhs, rhs)


/**
  * A trait which contains low priority implicits to be mixed within Domain.
  */
trait LowPriorityImplicitDomains:
  /**
    * An implicit domain obtained by an ordering, taking the max to be the upper bound operator.
    */
  implicit def orderingIsDomain[A](implicit o: Ordering[A]): Domain[A] = new Domain[A] {
    def lteq(x: A, y: A): Boolean = o.lteq(x, y)

    def tryCompare(x: A, y: A): Some[Int] = o.tryCompare(x, y)

    def upperBound(x: A, y: A): A = if o.compare(x, y) <= 0 then y else x
  }

object Domain extends LowPriorityImplicitDomains:
  /**
    * Add a syntactic sugar to easily get the current implicit Domain.
    */
  def apply[A](implicit dom: Domain[A]): Domain[A] = dom

  /**
    * An implicit conversion from `A` to `Domain[A].Ops`, which allows the seamsless
    * use of the infix operators.
    */
  implicit def infixDomainOps[A](a: A)(implicit dom: Domain[A]): Domain[A]#Ops = new dom.Ops(a)
