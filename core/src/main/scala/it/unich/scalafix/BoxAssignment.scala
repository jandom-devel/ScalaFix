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

import scala.language.implicitConversions

/**
  * A BoxAssignment maps a subset of the program's points to a Box. When `isDefinedAt(u)`
  * is false for a given unknown `u`, the corresponding `apply(u)` should be a right box.
  *
  * Like it was the case for Box, a BoxAssignent is also a blueprint for buildind equivalent
  * BoxAssignments. Each BoxAssignmant has a copy method which should produce a functionally
  * equivalent copy of `this`. The copy method should try to minimize object duplication.
  */
abstract class BoxAssignment[-U, V] extends PartialFunction[U, Box[V]] {
  outer =>

  /**
    * Returns true if the assignment is empty, i.e., it is undefined for all program points.
    */
  def isEmpty: Boolean

  /**
    * Returns true if all returned boxes are idempotent.
    */
  def boxesAreIdempotent: Boolean

  /**
    * Returns true if all boxes are right boxes.
    */
  def boxesAreRight: Boolean

  /**
    * Returns true if all boxes are immutable
    */
  def boxesAreImmutable: Boolean

  /**
    * Returns a copy of this box assignment. An immutable box assignment may just returns itself, but a mutable one
    * should produce a copy of itself.
    */
  def copy: BoxAssignment[U, V]

  /**
    * Restrict the domain of this box assignment. The new domain is the intersection of
    * the old domain and the set whose characteristic function is `domain`
    */
  def restrict[U1 <: U](domain: U1 => Boolean): BoxAssignment[U1,V] =
    if (isEmpty)
      this
    else
      new BoxAssignment.RestrictAssignment(this, domain)
}

/**
  * The `BoxAssignment` object defines factories for building box assignments.
  */
object BoxAssignment {

  private object EmptyAssigment extends BoxAssignment[Any, Any] {
    def apply(u: Any) = Box.right[Any]

    def isDefinedAt(u: Any) = false

    def isEmpty = true

    def boxesAreIdempotent = true

    def boxesAreRight = true

    def boxesAreImmutable = true

    def copy = this
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

    def boxesAreIdempotent = box.isIdempotent

    def boxesAreRight = box.isRight

    def boxesAreImmutable = box.isImmutable

    def copy = if (boxesAreImmutable) this else new ConstantAssignment(box.copy)
  }

  /**
    * A box assignment which returns a copy of the same box for each program point.
    *
    * @tparam V the type of values
    * @param box the template for the box we need to associate to program points
    *
    */
  private final class TemplateAssignment[V](box: Box[V]) extends BoxAssignment[Any, V] {
    private val hash = scala.collection.mutable.Map.empty[Any, Box[V]]

    def isDefinedAt(u: Any) = true

    def apply(u: Any) = hash.getOrElseUpdate(u, box.copy)

    def isEmpty = false

    def boxesAreIdempotent = box.isIdempotent

    def boxesAreImmutable = box.isImmutable

    def boxesAreRight = box.isRight

    def copy = if (boxesAreImmutable) this else new TemplateAssignment(box)
  }

  /**
    * A box assignment which restrict the assignment boxes to the set of program points which satisfy domain.
    *
    * @tparam U the type for program points of the box assignment
    * @tparam V the type of values
    * @tparam U1 the type of program points (subtype of U) to which we want to restrict the assignment
    * @param boxes the original box assignment
    * @param domain the set of points on which we want to use a box

    */
  private final class RestrictAssignment[U, V, U1 <: U](boxes: BoxAssignment[U, V], domain: U1 => Boolean) extends BoxAssignment[U1,V] {
    def apply(u: U1) = if (domain(u)) boxes(u) else Box.right[V]

    def isDefinedAt(u: U1) = domain(u) && boxes.isDefinedAt(u)

    def isEmpty = boxes.isEmpty

    def boxesAreIdempotent = boxes.boxesAreIdempotent

    def boxesAreImmutable = boxes.boxesAreImmutable

    def boxesAreRight = boxes.boxesAreRight

    def copy = if (boxesAreImmutable) this else new RestrictAssignment(boxes.copy, domain)
  }

  private final class WarrowingAssignment[U,V: PartialOrdering](widenings: BoxAssignment[U,V], narrowings: BoxAssignment[U,V]) extends BoxAssignment[U,V] {
    def apply(u: U) = Box.warrowing(widenings(u), narrowings(u))

    def isDefinedAt(x: U) = widenings.isDefinedAt(x) || narrowings.isDefinedAt(x)

    def isEmpty = widenings.isEmpty && narrowings.isEmpty

    def boxesAreIdempotent = false

    def boxesAreImmutable = widenings.boxesAreImmutable && narrowings.boxesAreImmutable

    def boxesAreRight = widenings.boxesAreRight && narrowings.boxesAreRight

    def copy = if (boxesAreImmutable) this else new WarrowingAssignment(widenings.copy, narrowings.copy)
  }

  /**
    * A box assignment which returns the same box for each program point. If box is mutable, different copies
    * are used for the different program points.
    *
    * @tparam V the type of values
    * @param box the template for box to be returned at each program point
    */
  implicit def apply[V](box: Box[V]): BoxAssignment[Any, V] = {
    if (box.isImmutable)
      new ConstantAssignment(box)
    else
      new TemplateAssignment(box)
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


  /**
    * A warrowing assignment obtained by combining the given widenings and narrowings, as defined in the paper:
    * Amato, Scozzari, Seidl, Apinis, Vodjani
    * "Efficiently intertwining widenings and narrowings".
    * Science of Computer Programming
    *
    * @tparam U the type of program points of the assignment
    * @tparam V the type of values, should be endowed with a partial ordering
    * @param widenings  widening assignment over U and V
    * @param narrowings narrowing assignment over U and V
    */
  def warrowing[U, V: PartialOrdering](widenings: BoxAssignment[U,V], narrowings: BoxAssignment[U,V]): BoxAssignment[U,V] = {
    if (widenings.boxesAreRight && narrowings.boxesAreRight)
      Box.right[V]
    else
      new WarrowingAssignment(widenings, narrowings)
  }
}
