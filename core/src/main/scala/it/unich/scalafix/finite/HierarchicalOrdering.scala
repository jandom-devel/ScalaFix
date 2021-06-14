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
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.finite

import scala.collection.mutable

/**
  * Hierarchical ordering as defined in Bourdoncle's paper "Efficient chaotic iteration strategies with widenings", FMPA'93.
  */
abstract class HierarchicalOrdering[N] extends GraphOrdering[N] {

  import HierarchicalOrdering.*

  /**
    * A sequence of elements and parenthesis representing the hierarchical ordering.
    */
  def toSeqWithParenthesis: Seq[HOElement[N]]

  /**
    * Converts a hierarchical ordering into a string on the basis of its parenthesized sequence
    */
  override def toString: String = toSeqWithParenthesis.mkString(stringPrefix, " ", "")
}

/**
  * The companion class for a hierarchical ordering contains the definition of the `Element` class and some
  * factory methods.
  */
object HierarchicalOrdering {

  /**
    * An HOElement[N] is either `Left` (left parenthesis), `Right` (right parenthesis) or `Val(x)` where
    * `x` is a value of type `N`. A sequence of HOElements is the standard representation of a hierarchical ordering.
    */
  sealed abstract class HOElement[+N]

  case object Left extends HOElement[Nothing] {
    override def toString = "("
  }

  case object Right extends HOElement[Nothing] {
    override def toString = ")"
  }

  case class Val[N](u: N) extends HOElement[N] {
    override def toString: String = u.toString
  }

  /**
    * Check if `seq` is a correct parenthesized sequence of elements.
    *
    * @param seq a sequence of HOElements.
    */
  private def validateSeqWithParenthesis[N](seq: IterableOnce[HOElement[N]]): Boolean = {
    var opened = 0
    var lastopened = false
    val it = seq.iterator
    while (it.hasNext) {
      val s = it.next()
      if (lastopened && !s.isInstanceOf[Val[?]]) return false
      if (s == Left) {
        opened += 1
        lastopened = true
      } else if (s == Right) {
        opened -= 1
        lastopened = false
        if (opened < 0) return false
      } else {
        lastopened = false
      }
    }
    opened == 0
  }

  /**
    * Builds a hierarchical ordering from a sequence of HOElements.
    */
  def apply[N](els: HOElement[N]*): HierarchicalOrdering[N] = new SequenceBasedHierarchicalOrdering(els.toIndexedSeq)

  /**
    * Builds a hierarchical ordering from a graph ordering. Components are opened for each head, and they are all
    * closed at the end. If `o` is the DFO for a graph, the result is a weak-topological ordering
    * for the same graph.
    */
  def apply[N](o: GraphOrdering[N]): HierarchicalOrdering[N] = new GraphOrderingBasedHO(o)

  /**
    * A hierarchical ordering defined by a sequence of HOElements.
    */
  private final class SequenceBasedHierarchicalOrdering[N](seq: IndexedSeq[HOElement[N]]) extends HierarchicalOrdering[N] {
    require(validateSeqWithParenthesis(seq), "Invalid sequence of elements and parenthesis")

    val stringPrefix = "HierarchicalOrdering"

    // TODO: check if this may be done faster
    private lazy val orderingIndex: Map[N, Int] = (for {
      (x, i) <- seq.zipWithIndex
      if x.isInstanceOf[Val[?]]
      Val(u) = x: @unchecked
    } yield u -> i).to(Map)

    def toSeq: Seq[N] = for {
      x <- seq
      if x.isInstanceOf[Val[?]]
      Val(u) = x: @unchecked
    } yield u

    def toSeqWithParenthesis: Seq[HOElement[N]] = seq

    def isHead(x: N): Boolean = seq.indices exists { i => seq(i) == Left && seq(i + 1) == Val(x) }

    def compare(x: N, y: N): Int = orderingIndex(x) - orderingIndex(y)
  }

  /**
    * A hierarchical ordering specified by a GraphOrdering. Components are opened for each head, and they are all
    * closed at the end. If the `o` is the DFO for a graph, the result is a weak-topological ordering for the same
    * graph.
    */
  private final class GraphOrderingBasedHO[N](o: GraphOrdering[N]) extends HierarchicalOrdering[N] {
    val stringPrefix = "HierarchicalOrdering"

    def toSeq: Seq[N] = o.toSeq

    def isHead(x: N): Boolean = o.isHead(x)

    def compare(x: N, y: N): Int = o.compare(x, y)

    lazy val toSeqWithParenthesis: Seq[HOElement[N]] = {
      val buffer = mutable.Buffer.empty[HOElement[N]]
      var open = 0
      for (x <- o.toSeq) {
        if (o.isHead(x)) {
          buffer.append(Left)
          open += 1
        }
        buffer.append(Val(x))
      }
      for (_ <- 0 until open) {
        buffer.append(Right)
      }
      buffer.toSeq
    }
  }

}
