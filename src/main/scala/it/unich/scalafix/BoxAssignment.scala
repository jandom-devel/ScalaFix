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

import it.unich.scalafix.BoxAssignment.RestrictAssignment
import it.unich.scalafix.lattice.Domain$

import scala.language.implicitConversions

/**
  * A BoxAssignment maps a subset of the program's points to a Box. When `isDefinedAt(u)`
  * is false for a given unknown `u`, the corresponding `apply(u)` should be a right box.
  */
abstract class BoxAssignment[-U, V] extends PartialFunction[U, Box[V]] {
  outer =>

  /**
    * Returns true if the assignment is empty, i.e., it is undefined for all program points.
    */
  def isEmpty: Boolean

  /**
    * Returns true if all returned boxes are idempotent
    */
  def areIdempotent: Boolean

  /**
    * Restrict the domain of this box assignment. The new domain is the intersection of
    * the old domain and the set whose characteristic function is `domain`
    */
  def restrict[U1 <: U](domain: U1 => Boolean): BoxAssignment[U1,V] =
    if (isEmpty)
      this
    else
      new RestrictAssignment(this, domain)
}

/**
  * The `BoxAssignment` object defines factories for building box assignments.
  */
object BoxAssignment {

  private object EmptyAssigment extends BoxAssignment[Any, Any] {
    def apply(u: Any) = Box.right[Any]

    def isDefinedAt(u: Any) = false

    def isEmpty = true

    def areIdempotent = true
  }

  /**
    * A constant box assignment maps the same box to all program points. Be careful because
    * if box has internal state, this is shared among all program points. For example, this
    * is not suited for delayed widenings or narrowings.
    *
    * @tparam V the type of the values
    * @param box the box to return for each program point
    */
  private final class ConstantAssignment[V](box: Box[V]) extends BoxAssignment[Any, V] {
    def isDefinedAt(u: Any) = true

    def apply(u: Any) = box

    def isEmpty = false

    def areIdempotent = box.isIdempotent
  }

  /**
    * A box assignment which returns a box according to the template boxTemplate.
    *
    * @tparam V the type of values
    * @param boxTemplate the template for the box we need to associate to program points
    *
    */
  private final class TemplateAssignment[V](boxTemplate: Box[V]) extends BoxAssignment[Any, V] {
    private val hash = scala.collection.mutable.Map.empty[Any, Box[V]]

    def isDefinedAt(u: Any) = true

    def apply(u: Any) = hash.getOrElseUpdate(u, boxTemplate.copy)

    def isEmpty = false

    def areIdempotent = boxTemplate.isIdempotent
  }

  /**
    * A box assignment which returns a b
 *
    * @param boxAssn
    * @param domain
    * @tparam U
    * @tparam V
    * @tparam U1
    */
  private final class RestrictAssignment[U, V, U1 <: U](boxAssn: BoxAssignment[U, V], domain: U1 => Boolean) extends BoxAssignment[U1,V] {
    def apply(u: U1) = if (domain(u)) boxAssn(u) else Box.right[V]

    def isDefinedAt(u: U1) = domain(u) && boxAssn.isDefinedAt(u)

    def isEmpty = false

    def areIdempotent = boxAssn.areIdempotent
  }

  /**
    * A box assignment which returns a box for each program point. Box is taken to be a template,
    * and different copies of it for each program point are generated in case it is mutable.
    *
    * @tparam V the type of values
    * @param boxTemplate the template for box to be returned at each program point
    */
  implicit def apply[V](boxTemplate: Box[V]): BoxAssignment[Any, V] = {
    if (boxTemplate.isImmutable)
      new ConstantAssignment(boxTemplate)
    else
      new TemplateAssignment(boxTemplate)
  }

  /**
    * A box assignment which returns the immutable and idempotent box corresponding to the map  `f: (V,V) => V`,
    * for each program point.
    */
  implicit def apply[V](f: Function2[V, V, V]): BoxAssignment[Any, V] =
    new ConstantAssignment[V](Box(f))

  /**
    * A box assignment which is undefined for each program point.
    */
  def empty[V]: BoxAssignment[Any, V] = EmptyAssigment.asInstanceOf[BoxAssignment[Any, V]]

}
