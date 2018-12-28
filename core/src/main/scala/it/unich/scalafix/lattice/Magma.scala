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

package it.unich.scalafix.lattice

import scala.language.implicitConversions

/**
  * A magma is a set with a binary operation. The magma trait is a type class for magmas,
  * which defines a single binary operation called `op`.
  * If an implicit object of type `Magma[A]` is in scope, then a binary operator `|+|`
  * corresponding to the magma operation is available for objects of type `A`.
  */
trait Magma[A] {
  /**
    * The magma  operation.
    */
  def op(x: A, y: A): A

  class Ops(lhs: A) {
    def |+|(rhs: A): A = op(lhs, rhs)
  }
}

object Magma {
  /**
    * Add syntactic sugar to easily get the current implicit Magma.
    */
  def apply[A](implicit magma: Magma[A]) = magma

  /**
    * An implicit magma whose op operator corresponds to the upper bound of a directed partial ordering.
    */
  implicit def domainIsMagma[A](implicit dom: Domain[A]) = new Magma[A] {
    def op(x: A, y:A) = dom.upperBound(x,y)
  }

  /**
    * An implicit conversion from `A` to `Magma[A].Ops`, which allows the seamsless
    * use of the `|+|` operator.
    */
  implicit def infixMagmaOps[A](a: A)(implicit magma: Magma[A]): Magma[A]#Ops = new magma.Ops(a)
}
