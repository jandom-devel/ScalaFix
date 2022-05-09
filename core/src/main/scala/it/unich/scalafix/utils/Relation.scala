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

package it.unich.scalafix.utils

import scala.annotation.targetName
import scala.collection.mutable

/**
 * A mathematical relation over types `A` and `B`.
 */
opaque type Relation[-A, B] = A => Set[B]

/**
 * Collection of private classes, factories and extension methods for the
 * `Relation` type
 */
object Relation:

  private class WithDiagonal[A](r: Relation[A, A]) extends Relation[A, A]:
    def apply(a: A) = r(a) + a

  private class FromHash[A, B](hash: collection.Map[A, Set[B]]) extends Relation[A, B]:
    def apply(a: A) = hash.getOrElse(a, Set.empty[B])

  /** Returns a relation given specified a function `A => Set[A]`. */
  def apply[A, B](f: A => Set[B]): Relation[A, B] = f

  /**
   * Returns an influence relation specified by a map `hash`. For elements which
   * are not in the keyset of `hash`, the relation returns the empty set.
   */
  def apply[A, B](hash: Map[A, Set[B]]): Relation[A, B] = FromHash(hash)

  /**
   * Returns a relation specified by its graph, i.e., iterable of pairs `a ->
   * b`, each meaning that `a` is in relation with `b`. When iterating over the
   * image of `a`, elements are guaranteed to be returned in the order in which
   * they appear in graph.
   */
  def apply[A, B](graph: Iterable[(A, B)]): Relation[A, B] =
    val hash = mutable.HashMap.empty[A, Set[B]]
    for (u, v) <- graph do hash(u) = hash.getOrElse(u, Set.empty[B]) + v
    FromHash(hash)

  /**
   * Returns a relation by its graph, i.e., the specified pairs `a -> b`, each
   * meaning that `a` is in relation with `b`. When iterating over the image of
   * `u`, elements are guaranteed to be returned in the order in which they
   * appear in graph.
   */
  def apply[A, B](graph: (A, B)*): Relation[A, B] = apply(graph)

  /**
   * Returns a relation specified by the given pairs `a -> X`, each meaning that
   * `a` is in relation with all the elements in the set `X`.
   */
  @targetName("applySet")
  def apply[A, B](graph: (A, Set[B])*): Relation[A, B] = FromHash(Map(graph*))

  extension [A](rel: Relation[A, A])
    /**
     * Returns a new relation where each element `a` is in relation with itself
     * (in addition to the other elements it is already in relation with).
     */
    def withDiagonal: Relation[A, A] = WithDiagonal(rel)

  extension [A, B](rel: Relation[A, B])
    /**
     * Returns the set of elements in relation with `a`.
     */
    inline def apply(a: A): Set[B] = rel.apply(a)
