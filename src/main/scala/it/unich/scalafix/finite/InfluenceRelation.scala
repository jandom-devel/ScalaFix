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

package it.unich.scalafix.finite

import scala.collection.mutable

/**
  * An influence relation maps an unknowns to set of unknowns it influences in the equation
  * system.
  *
  * @tparam U the type for unknowns
  */
abstract class InfluenceRelation[U] extends Function[U, collection.Set[U]] {
  /**
    * Add the influence of an unknown `u` to itself
    */
  def withDiagonal: InfluenceRelation[U]
}

/**
  * The `InfluenceRelation` object contains factory methods and concrete implementations.
  */
object InfluenceRelation {

  /**
    * A trait for InfluenceRelation which provides the implementation of the `withDiagonal` method.
    */
  trait WithDiagonal[U] {
    this: InfluenceRelation[U] =>

    def withDiagonal: InfluenceRelation[U] = new InfluenceWithDiagonal(this)
  }

  private class InfluenceWithDiagonal[U](infl: InfluenceRelation[U]) extends InfluenceRelation[U] {
    def apply(u: U) = infl(u) + u

    def withDiagonal = this
  }

  private class InfluenceRelationFromFunction[U](f: U => Set[U]) extends InfluenceRelation[U] with WithDiagonal[U] {
    def apply(u: U) = f(u)
  }

  private class InfluenceRelationFromHash[U](hash: Map[U, collection.Set[U]]) extends InfluenceRelation[U] with WithDiagonal[U] {
    def apply(u: U) = hash.getOrElse(u, Set.empty[U])
  }

  /**
    * Returns an influence relation given a set of unknowns (the domain) and a function `U => Set[U]`.
    */
  def apply[U](f: U => Set[U]): InfluenceRelation[U] = new InfluenceRelationFromFunction(f)

  /**
    * Returns an influence relation given a map `hash`. For unknowns which are not in the keyset of hash, i t returns
    * the empty set.
    */
  def apply[U](hash: Map[U, Set[U]]): InfluenceRelation[U] = new InfluenceRelationFromHash(hash)

  /**
    * Returns an influence relation given a traversable collection of pairs `(u -> v)`, each pair meaning that `u`
    * influences `v`. When iterating over the influence set of `u`, elements are guaranteed to be returned in the
    * order in which they appear in graph.
    */
  def apply[U](graph: TraversableOnce[(U, U)]): InfluenceRelation[U] = {
    val hash = new mutable.HashMap[U, mutable.Set[U]] with mutable.MultiMap[U, U] {
      override def makeSet = new mutable.LinkedHashSet[U]
    }
    for ((u, v) <- graph) hash.addBinding(u, v)
    new InfluenceRelationFromHash[U](hash.toMap)
  }
}
