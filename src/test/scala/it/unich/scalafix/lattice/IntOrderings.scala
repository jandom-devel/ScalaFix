/**
  * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.scalafix.Box

object IntOrderings {

  case class IntDirectedSet(v: Int) extends DirectedPartiallyOrdered[IntDirectedSet] {
    def upperBound[B >: IntDirectedSet](that: B)(implicit evidence: (B) => DirectedPartiallyOrdered[B]): B = that match {
      case that: IntDirectedSet => IntDirectedSet(that.v max v)
      case _ => that upperBound this
    }

    override def tryCompareTo[B >: IntDirectedSet](that: B)(implicit evidence: (B) => PartiallyOrdered[B]): Option[Int] = that match {
      case that: IntDirectedSet => Some(v - that.v)
      case _ => that tryCompareTo this
    }
  }

  implicit object IntOrdering extends DirectedPartialOrdering[Int] {
    def upperBound(x: Int, y: Int): Int = x max y

    def tryCompare(x: Int, y: Int): Option[Int] = Some(x - y)

    def lteq(x: Int, y: Int): Boolean = x <= y
  }

  val intwidening: Box[Int] = { (x: Int, y: Int) => if (x >= y) x else Int.MaxValue }

  val intnarrowing: Box[Int] = { (x: Int, y: Int) => if (x == Int.MaxValue) y else x }
}
