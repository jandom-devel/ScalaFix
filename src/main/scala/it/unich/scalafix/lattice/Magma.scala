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
  * A magma is a set with a binary operation. The magma trait is a typeclass for magmas,
  * which defines a single binary operation called `op`.
  * If an implicit object of type `Magma[A]` is available, then a binary operator `|+|`
  * corresponding to the magma operation is available for objects of type `A`.
  */
trait Magma[A] {
  /**
    * The magma  operation.
    */
  def op(x: A, y: A): A

  class Ops(val lhs: A) {
    def |+|(rhs: A): A = op(lhs, rhs)
  }
}

object Magma {
  implicit def magmaOps[A](a: A)(implicit magma: Magma[A]) = new magma.Ops(a)
}
