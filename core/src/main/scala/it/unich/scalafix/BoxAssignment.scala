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

/**
 * A ComboAssignment maps a subset of unknowns to a Combo. When `isDefinedAt(u)` is
 * false for a given unknown `u`, the corresponding `apply(u)` should be a right
 * combo.
 *
 * Like it was the case for Combo, a ComboAssignent is also a blueprint for buildind
 * equivalent ComboAssignments. Each ComboAssignmant has a copy method which should
 * produce a functionally equivalent copy of `this`. The copy method should try
 * to minimize object duplication.
 */
abstract class ComboAssignment[-U, V] extends PartialFunction[U, Combo[V]]:
  /**
   * Returns true if the assignment is empty, i.e., it is undefined for all
   * program points.
   */
  def isEmpty: Boolean

  /** Returns true if all returned combos are idempotent. */
  def combosAreIdempotent: Boolean

  /** Returns true if all combos are right combos. */
  def combosAreRight: Boolean

  /** Returns true if all combos are immutable */
  def combosAreImmutable: Boolean

  /**
   * Returns a copy of this combo assignment. An immutable combo assignment may just
   * returns itself, but a mutable one should produce a copy of itself.
   */
  def copy: ComboAssignment[U, V]

  /**
   * Restrict the domain of this combo assignment. The new domain is the
   * intersection of the old domain and the set whose characteristic function is
   * `domain`
   */
  def restrict[U1 <: U](domain: U1 => Boolean): ComboAssignment[U1, V] =
    if isEmpty then this
    else ComboAssignment.RestrictAssignment(this, domain)

/**
 * The `ComboAssignment` object defines factories for building combo assignments.
 */
object ComboAssignment:

  private object EmptyAssigment extends ComboAssignment[Any, Any]:
    def apply(u: Any): Combo.ImmutableCombo[Any] = Combo.right[Any]
    def isDefinedAt(u: Any) = false
    def isEmpty = true
    def combosAreIdempotent = true
    def combosAreRight = true
    def combosAreImmutable = true
    def copy: this.type = this

  /**
   * A constant combo assignment maps the same combo to all program points. Be
   * careful because if combo has internal state, this is shared among all program
   * points. For example, this is not suited for delayed widenings or
   * narrowings.
   *
   * @tparam V
   *   the type of the values
   * @param combo
   *   the combo to return for each program point
   */
  private final class ConstantAssignment[V](combo: Combo[V]) extends ComboAssignment[Any, V]:
    def isDefinedAt(u: Any) = true
    def apply(u: Any): Combo[V] = combo
    def isEmpty = false
    def combosAreIdempotent: Boolean = combo.isIdempotent
    def combosAreRight: Boolean = combo.isRight
    def combosAreImmutable: Boolean = combo.isImmutable
    def copy: ConstantAssignment[V] =
      if combosAreImmutable then this else ConstantAssignment(combo.copy)

  /**
   * A combo assignment which returns a copy of the same combo for each program
   * point.
   *
   * @tparam V
   *   the type of values
   * @param combo
   *   the template for the combo we need to associate to program points
   */
  private final class TemplateAssignment[V](combo: Combo[V]) extends ComboAssignment[Any, V]:
    private val hash = scala.collection.mutable.Map.empty[Any, Combo[V]]
    def isDefinedAt(u: Any) = true
    def apply(u: Any): Combo[V] = hash.getOrElseUpdate(u, combo.copy)
    def isEmpty = false
    def combosAreIdempotent: Boolean = combo.isIdempotent
    def combosAreImmutable: Boolean = combo.isImmutable
    def combosAreRight: Boolean = combo.isRight
    def copy: TemplateAssignment[V] = if combosAreImmutable then this else TemplateAssignment(combo)

  /**
   * A combo assignment which restrict the assignment combos to the set of program
   * points which satisfy domain.
   *
   * @tparam U
   *   the type for program points of the combo assignment
   * @tparam V
   *   the type of values
   * @tparam U1
   *   the type of program points (subtype of U) to which we want to restrict
   *   the assignment
   * @param combos
   *   the original combo assignment
   * @param domain
   *   the set of points on which we want to use a combo
   */
  private final class RestrictAssignment[U, V, U1 <: U](
      combos: ComboAssignment[U, V],
      domain: U1 => Boolean
  ) extends ComboAssignment[U1, V]:
    def apply(u: U1): Combo[V] = if domain(u) then combos(u) else Combo.right[V]
    def isDefinedAt(u: U1): Boolean = domain(u) && combos.isDefinedAt(u)
    def isEmpty: Boolean = combos.isEmpty
    def combosAreIdempotent: Boolean = combos.combosAreIdempotent
    def combosAreImmutable: Boolean = combos.combosAreImmutable
    def combosAreRight: Boolean = combos.combosAreRight
    def copy: RestrictAssignment[U, V, U1] =
      if combosAreImmutable then this else RestrictAssignment(combos.copy, domain)

  private final class WarrowingAssignment[U, V: PartialOrdering](
      widenings: ComboAssignment[U, V],
      narrowings: ComboAssignment[U, V]
  ) extends ComboAssignment[U, V]:
    def apply(u: U): Combo[V] = Combo.warrowing(widenings(u), narrowings(u))
    def isDefinedAt(x: U): Boolean = widenings.isDefinedAt(x) || narrowings.isDefinedAt(x)
    def isEmpty: Boolean = widenings.isEmpty && narrowings.isEmpty
    def combosAreIdempotent = false
    def combosAreImmutable: Boolean = widenings.combosAreImmutable && narrowings.combosAreImmutable
    def combosAreRight: Boolean = widenings.combosAreRight && narrowings.combosAreRight
    def copy: WarrowingAssignment[U, V] =
      if combosAreImmutable then this else WarrowingAssignment(widenings.copy, narrowings.copy)

  /**
   * A combo assignment which returns the same combo for each program point. If combo
   * is mutable, different copies are used for the different program points.
   *
   * @tparam V
   *   the type of values
   * @param combo
   *   the template for combo to be returned at each program point
   */
  def apply[V](combo: Combo[V]): ComboAssignment[Any, V] =
    if combo.isImmutable then ConstantAssignment(combo)
    else TemplateAssignment(combo)

  /**
   * A combo assignment which returns the immutable and idempotent combo
   * corresponding to the map `f: (V,V) => V`, for each program point.
   */
  def apply[V](f: (V, V) => V, isIdempotent: Boolean = true): ComboAssignment[Any, V] =
    ConstantAssignment[V](Combo(f, isIdempotent))

  /** A combo assignment which is undefined for each program point. */
  def empty[V]: ComboAssignment[Any, V] = EmptyAssigment.asInstanceOf[ComboAssignment[Any, V]]

  /**
   * A warrowing assignment obtained by combining the given widenings and
   * narrowings, as defined in the paper: Amato, Scozzari, Seidl, Apinis,
   * Vodjani "Efficiently intertwining widenings and narrowings". Science of
   * Computer Programming
   *
   * @tparam U
   *   the type of program points of the assignment
   * @tparam V
   *   the type of values, should be endowed with a partial ordering
   * @param widenings
   *   widening assignment over U and V
   * @param narrowings
   *   narrowing assignment over U and V
   */
  def warrowing[U, V: PartialOrdering](
      widenings: ComboAssignment[U, V],
      narrowings: ComboAssignment[U, V]
  ): ComboAssignment[U, V] =
    if widenings.combosAreRight && narrowings.combosAreRight then ComboAssignment(Combo.right[V])
    else WarrowingAssignment(widenings, narrowings)
