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

package it.unich.scalafix.finite

/**
 * An ordering of the unknowns of a finite equation system.
 *
 * For each unknown, we mark whether it is an head element or not. When an
 * equation system is defined over a domain of values which does not enjoy the
 * ascending chain condition, head unknowns should be evaluated with the use of
 * a combo in order to ensure convergence.
 *
 * @tparam U
 *   the type for the unknowns
 */
trait UnknownOrdering[U] extends Ordering[U]:

  /** Returns the ordered sequence of unknowns. */
  def toSeq: Seq[U]

  /** It returns whether `u` is an head element. */
  def isHead(u: U): Boolean

  /** Defines the prefix of this object's `toString` representation. */
  protected def stringPrefix = "UnknownOrdering"

  /**
   * Converts an `UnknownOrdering` into a string composed by the sequence of its
   * elements in the correct order. Head elements are marked with parenthesis.
   */
  override def toString: String =
    stringPrefix + (toSeq map { x => if isHead(x) then s"($x)" else x })
      .mkString("( ", " ", " )")

/** Collection of private classes and factory methods for unknown orderings. */
object UnknownOrdering:

  /**
   * An unknown ordering where each element is an head, and the order is given
   * by the sequence `seq`.
   */
  private final class TrivialUnknownOrdering[N](seq: Seq[N]) extends UnknownOrdering[N]:

    override def toSeq: Seq[N] = seq

    override def isHead(x: N): Boolean = true

    /**
     * @inheritdoc
     *
     * @todo
     *   the implementation is inefficient, since it is based on finding the
     *   unknowns in the sequence returned by `toSeq`.
     */
    override def compare(x: N, y: N): Int = seq.indexOf(x) - seq.indexOf(y)

  /**
   * Build an unknown ordering where each element is an head, and the order is
   * given by the input sequence.
   */
  def apply[N](elements: N*): UnknownOrdering[N] = TrivialUnknownOrdering(elements)
