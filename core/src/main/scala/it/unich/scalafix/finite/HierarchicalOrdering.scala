/**
 * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
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
 * Hierarchical ordering of unknowns, as defined in Bourdoncle's paper
 * "Efficient chaotic iteration strategies with widenings", FMPA'93.
 *
 * @tparam U
 *   the type for the unknowns
 */
trait HierarchicalOrdering[U] extends UnknownOrdering[U]:

  /**
   * Returns a sequence of elements and parenthesis representing the
   * hierarchical ordering.
   */
  def toSeqWithParenthesis: Seq[HierarchicalOrdering.HOElement[U]]

  override protected def stringPrefix = "HierarchicalOrdering"

  /**
   * Converts a hierarchical ordering into a string on the basis of its
   * parenthesized sequence
   */
  override def toString: String = toSeqWithParenthesis.mkString(stringPrefix, " ", "")

/**
 * The companion class for a hierarchical ordering contains the definition of
 * the `HOElement` class and some factory methods.
 */
object HierarchicalOrdering:

  /**
   * An HOElement[U] is either `Left` (left parenthesis), `Right` (right
   * parenthesis) or `Val(x)` where `x` is a value of type `U`. A sequence of
   * HOElements is the standard representation of a hierarchical ordering.
   */
  sealed abstract class HOElement[+U]

  case object Left extends HOElement[Nothing]:
    override def toString = "("

  case object Right extends HOElement[Nothing]:
    override def toString = ")"

  case class Val[U](u: U) extends HOElement[U]:
    override def toString: String = u.toString

  /**
   * Check if `els` is a correct parenthesized sequence of elements.
   *
   * @param els
   *   a sequence of HOElements.
   */
  private def validateSeqWithParenthesis[U](els: IterableOnce[HOElement[U]]): Boolean =
    var opened = 0
    var lastopened = false
    val it = els.iterator
    while it.hasNext do
      val s = it.next()
      if lastopened && !s.isInstanceOf[Val[?]] then return false
      if s == Left then
        opened += 1
        lastopened = true
      else if s == Right then
        opened -= 1
        lastopened = false
        if opened < 0 then return false
      else lastopened = false
    opened == 0

  /** Builds a hierarchical ordering from a sequence of HOElements. */
  def apply[U](els: HOElement[U]*): HierarchicalOrdering[U] =
    SequenceBasedHierarchicalOrdering(els.toIndexedSeq)

  /**
   * Builds a hierarchical ordering from an unknown ordering. Components are
   * opened for each head, and they are all closed after the last unknown. If
   * `o` is the DFO for a graph, the result is a weak-topological ordering for
   * the same graph.
   */
  def apply[U](o: UnknownOrdering[U]): HierarchicalOrdering[U] =
    UnknownOrderingBasedHO(o)

  /** A hierarchical ordering defined by a sequence of HOElements. */
  private final class SequenceBasedHierarchicalOrdering[U](els: IndexedSeq[HOElement[U]])
      extends HierarchicalOrdering[U]:
    require(validateSeqWithParenthesis(els), "Invalid sequence of elements and parenthesis")

    // TODO: check if this may be done faster
    private lazy val orderingIndex: Map[U, Int] =
      val seqIndex: Seq[(U, Int)] = for
        (x, i) <- els.zipWithIndex
        if x.isInstanceOf[Val[?]]
        Val(u) = x.asInstanceOf[Val[U]]
      yield u -> i
      seqIndex.toMap

    override def toSeq: Seq[U] = for
      x <- els
      if x.isInstanceOf[Val[?]]
      Val(u) = x.asInstanceOf[Val[U]]
    yield u

    override def toSeqWithParenthesis: Seq[HOElement[U]] = els

    override def isHead(x: U): Boolean = els.indices exists { i => els(i) == Left && els(i + 1) == Val(x) }

    override def compare(x: U, y: U): Int = orderingIndex(x) - orderingIndex(y)

  /**
   * A hierarchical ordering specified by an unknown ordering. Components are
   * opened for each head, and they are all closed at the end. If the `o` is the
   * DFO for a graph, the result is a weak-topological ordering for the same
   * graph.
   */
  private final class UnknownOrderingBasedHO[N](o: UnknownOrdering[N])
      extends HierarchicalOrdering[N]:

    override def toSeq: Seq[N] = o.toSeq

    override def isHead(x: N): Boolean = o.isHead(x)

    override def compare(x: N, y: N): Int = o.compare(x, y)

    override lazy val toSeqWithParenthesis: Seq[HOElement[N]] =
      val buffer = mutable.Buffer.empty[HOElement[N]]
      var open = 0
      for x <- o.toSeq do
        if o.isHead(x) then
          buffer.append(Left)
          open += 1
        buffer.append(Val(x))
      for _ <- 0 until open do buffer.append(Right)
      buffer.toSeq
