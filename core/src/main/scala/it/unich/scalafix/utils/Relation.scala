/**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it>
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
 *
 * @note
 *   Although theoretically it would be more correct to define this type as `A
 *   \=> Set[B]`, this would force whoever calls ScalaFix to use sets for
 *   providing relations. If the the relation is already available in another
 *   format, this would limit data reusal.
 */
opaque type Relation[-A, +B] = A => Iterable[B]

/**
 * Collection of private classes, factories and extension methods for the
 * `Relation` type
 */
object Relation:

  private class WithDiagonal[A](r: Relation[A, A]) extends Relation[A, A]:
    def apply(a: A) = Iterable(a) ++ r(a)

  private class FromHash[A, B](hash: collection.Map[A, Iterable[B]]) extends Relation[A, B]:
    def apply(a: A) = hash.getOrElse(a, Iterable.empty[B])

  /** Returns a relation given specified a function `A => Seq[A]`. */
  def apply[A, B](f: A => Iterable[B]): Relation[A, B] = f

  /**
   * Returns an influence relation specified by a map `hash`. For elements which
   * are not in the keyset of `hash`, the relation returns the empty sequence.
   */
  def apply[A, B](hash: Map[A, Iterable[B]]): Relation[A, B] = FromHash(hash)

  /**
   * Returns a relation specified by its graph, i.e., iterable of pairs `a ->
   * b`, each meaning that `a` is in relation with `b`. When iterating over the
   * image of `a`, elements are guaranteed to be returned in the order in which
   * they appear in graph.
   */
  def apply[A, B](graph: IterableOnce[(A, B)]): Relation[A, B] =
    val hash = mutable.HashMap.empty[A, Seq[B]]
    for (u, v) <- graph.iterator do hash(u) = hash.getOrElse(u, Seq.empty[B]) :+ v
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
   * `a` is in relation with all the elements in the sequence `X`.
   */
  @targetName("applyIterable")
  def apply[A, B](graph: (A, Iterable[B])*): Relation[A, B] = FromHash(Map(graph*))

  extension [A](rel: Relation[A, A])
    /**
     * Returns a new relation where each element `a` is in relation with itself
     * (in addition to the other elements it is already in relation with).
     */
    def withDiagonal: Relation[A, A] = WithDiagonal(rel)

  extension [A, B](rel: Relation[A, B])
    /** Returns the sequence of elements in relation with `a`. */
    inline def apply(a: A): Iterable[B] = rel.apply(a)
