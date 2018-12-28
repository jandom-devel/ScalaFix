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

package it.unich.scalafix.utils

import scala.collection.mutable

/**
  * This trait represents an arbitrary mathematical relation over values of type `A`.
  * It extends `A => collection.Set[A]` and the apply method is guaranteed to return a set (eventually empty)
  * for each element of type `A`.
  *
  * @tparam A the domain of the relation.
  */
trait Relation[A] extends (A => collection.Set[A]) {
  import Relation._

  /**
    * Returns a new relation where each element is in relation with itself.
    */
  def withDiagonal: Relation[A] = new InfluenceWithDiagonal(this)
}

/**
  * The `Relation` object contains factory methods and concrete implementations.
  */
object Relation {


  private class InfluenceWithDiagonal[A](r: Relation[A]) extends Relation[A] {
    def apply(x: A) = r(x) + x

    override def withDiagonal = this
  }

  private class InfluenceRelationFromFunction[A](f: A => collection.Set[A]) extends Relation[A] {
    def apply(x: A) = f(x)
  }

  private class InfluenceRelationFromHash[A](hash: collection.Map[A, collection.Set[A]]) extends Relation[A] {
    def apply(x: A) = hash.getOrElse(x, Set.empty[A])
  }

  /**
    * Returns an influence relation given a function `A => Set[A]`. To respect the contract of a `Relation`, the map
    * `f` should return a set for every possible value of `A`-
    */
  def apply[A](f: A => Set[A]): Relation[A] = new InfluenceRelationFromFunction(f)

  /**
    * Returns an influence relation given a map `hash`. For elements which are not in the keyset of `hash`,
    * it returns the empty set.
    */
  def apply[A](hash: collection.Map[A, Set[A]]): Relation[A] = new InfluenceRelationFromHash(hash)

  /**
    * Returns an relation given a traversable collection of pairs `(u -> v)`, each pair meaning that `u`
    * relates to `v`. When iterating over the image of `u`, elements are guaranteed to be returned in the
    * order in which they appear in graph.
    */
  def apply[A](graph: Seq[(A, A)]): Relation[A] = {
    val hash = new mutable.HashMap[A, mutable.Set[A]] with mutable.MultiMap[A, A] {
      override def makeSet = new mutable.LinkedHashSet[A]
    }
    for ((u, v) <- graph) hash.addBinding(u, v)
    new InfluenceRelationFromHash[A](hash)
  }
}
