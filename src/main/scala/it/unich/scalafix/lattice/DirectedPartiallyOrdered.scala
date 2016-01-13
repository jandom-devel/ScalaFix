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

/**
  * A class for data which has a directed partial ordered. Be careful of the difference between
  * `DirectedPartialOrdering` and `DirectedPartiallyOrdered`.
  */
trait DirectedPartiallyOrdered[+A] extends PartiallyOrdered[A] {
  /**
    * It returns an upper bound of `this` and `that`.
    */
  def upperBound[B >: A](that: B)(implicit evidence: (B) => DirectedPartiallyOrdered[B]): B
}
