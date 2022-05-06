/**
 * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.scalafix.finite

import scala.collection.mutable

/**
 * The influence relation among the unknowns of an equation system. It extends
 * `U => Set(U)`. Given an `u: U`, it returns the set of unknowns `v: U` which
 * are influenced by `u`.
 *
 * @tparam U
 *   type of the unknowns for this influence relation
 */
@FunctionalInterface
trait InfluenceRelation[U] extends (U => Set[U]):

  /**
   * Returns a new influence relation obtained by adding a self influence from
   * any unknown to itself.
   */
  def withDiagonal: InfluenceRelation[U] = InfluenceRelation.InfluenceWithDiagonal(this)

/**
 * Collection of private classes and factory methods for influence relations
 * among unknowns.
 */
object InfluenceRelation:

  private class InfluenceWithDiagonal[U](r: InfluenceRelation[U]) extends InfluenceRelation[U]:
    // TODO: check for faster implementation
    def apply(x: U) = r(x) + x

    override def withDiagonal: InfluenceRelation[U] = this

  private class InfluenceRelationFromHash[U](hash: collection.Map[U, Set[U]])
      extends InfluenceRelation[U]:
    def apply(x: U) = hash.getOrElse(x, Set.empty[U])

  /** Returns an influence relation given by a function `A => Set[A]`. */
  def apply[U](f: U => Set[U]): InfluenceRelation[U] = f.apply

  /**
   * Returns an influence relation given by a map `hash`. For elements which are
   * not in the keyset of `hash`, the influence relation returns the empty set.
   */
  def apply[U](hash: collection.Map[U, Set[U]]): InfluenceRelation[U] =
    InfluenceRelationFromHash(hash)

  /**
   * Returns a relation given an iterable of pairs `u -> v`, each meaning that
   * `u` influences `v`. When iterating over the image of `u`, elements are
   * guaranteed to be returned in the order in which they appear in graph.
   */
  def apply[U](graph: Iterable[(U, U)]): InfluenceRelation[U] =
    val hash = mutable.HashMap.empty[U, Set[U]]
    for (u, v) <- graph do hash(u) = hash.getOrElse(u, Set.empty[U]) + v
    InfluenceRelationFromHash[U](hash)

  /**
   * Returns a relation from the specified pairs `u -> v`, each meaning that `u`
   * influences `v`. When iterating over the image of `u`, elements are guaranteed
   * to be returned in the order in which they appear in graph.
   */
  def apply[U](graph: (U, U)*): InfluenceRelation[U] =
    apply(graph)
