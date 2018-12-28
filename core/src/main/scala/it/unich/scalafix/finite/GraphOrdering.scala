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

package it.unich.scalafix.finite

/**
  * A GraphOrdering is an ordering on objects of the type `N` (which should be thought of
  * as nodes of a graph), where for each object we mark whether it is an head element or not.
  * Note that a graph ordering generally considers only a subset of the elements of type `N`,
  * those returned by the `toSeq` method. The result of applying any method of this trait
  * on any element which is not part of the domain is not specified.
  *
  * @tparam N the type of the ordered element.
  */
abstract class GraphOrdering[N] extends Ordering[N] {
  /**
    * Returns the elements which are part of the ordering in the correct order.
    */
  def toSeq: Seq[N]

  /**
    * Defines the prefix of this object's toString representation.
    */
  def stringPrefix: String

  /**
    * It returns whether `n` is an head element.
    */
  def isHead(n: N): Boolean

  /**
    * Converts a GraphOrdering into a string composed by the sequence of its elements in the correct order. Head
    * elements are marked with parenthesis.
    */
  override def toString = stringPrefix + (toSeq map { x => if (isHead(x)) x.toString else "(" + x.toString + ")" }).mkString("( ", " ", " )")
}

object GraphOrdering {

  /**
    * A graph ordering where each element is an head, and the order is given by the sequence `elements`.
    */
  private final class TrivialGraphOrdering[N](seq: Seq[N]) extends GraphOrdering[N] {
    val stringPrefix = "GraphOrdering"

    def toSeq = seq

    def isHead(x: N) = true

    def compare(x: N, y: N) = seq.indexOf(x) - seq.indexOf(y)
  }

  /**
    * Build a graph ordering where each element is an head, and the order is given by the sequence of `elements`.
    * @todo Implementation is inefficient and can be improved using memoization
    */
  def apply[N](elements: N*): GraphOrdering[N] = new TrivialGraphOrdering(elements)
}
