/**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it> and
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

import it.unich.scalafix.utils.Relation

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * This class represents a depth-first ordering of a set of unknowns, as it
 * appears in the Aho, Sethi, Ullman book on compilers. It extends the concept
 * of unknown ordering, since it allows to classify an hypothetical influence
 * between unknowns in one of the three categories denoted as Advancing,
 * Retreating and Cross influence.
 *
 * @tparam U
 *   the type for the unknowns
 */
abstract class DFOrdering[U] extends UnknownOrdering[U]:

  import DFOrdering.InfluenceType

  /**
   * It returns the type of an influence u -> v.
   *
   * @param u
   *   source node
   * @param v
   *   target node
   */
  def influenceType(u: U, v: U): InfluenceType

/**
 * The companion class for a DFOrdering defines the required enumerations and
 * factory methods.
 */
object DFOrdering:

  /**
   * Influences may be of three different types: Advancing, Retreating and
   * Cross.
   */
  enum InfluenceType:
    case Advancing, Retreating, Cross

  /** Returns the DFOrdering for a finite equation system. */
  def apply[U](eqs: FiniteEquationSystem[U, ?, ?]): DFOrdering[U] =
    DFOrderingFromInfl[U](eqs.infl, eqs.unknowns, eqs.inputUnknowns)

  /**
   * Returns the DFOrdering for the graph of unknowns encoded by the influence
   * relation `infl`, the set of unknowns in `unknowns` and set of initial
   * unknowns in `inputUnknowns`.
   */
  def apply[N](
      infl: Relation[N, N],
      unknowns: Iterable[N],
      inputUnknowns: Iterable[N]
  ): DFOrdering[N] =
    DFOrderingFromInfl[N](infl, unknowns, inputUnknowns)

  /**
   * The DFOrdering for the graph of unknowns encoded by the influence relation
   * `infl`, the set of unknowns in `unknowns` and set of initial unknowns in
   * `inputUnknowns`.
   *
   * @param infl
   *   the influence relation between unknowns
   * @param the
   *   set of all unknowns
   * @param entries
   *   nodes from which to start the visit.
   */
  private final class DFOrderingFromInfl[U](
      infl: Relation[U, U],
      unknowns: Iterable[U],
      inputUnknowns: Iterable[U]
  ) extends DFOrdering[U]:

    import DFOrdering.InfluenceType.*

    // Internal computation
    private val dfn = mutable.HashMap.empty[U, Int]

    // Depth-First spanning tree
    private val dfst = mutable.Set.empty[(U, U)]

    // Set of heads
    private val heads = mutable.Set.empty[U]
    initDFO()

    def initDFO() =
      val visited = mutable.LinkedHashSet.empty[U]
      val stack = mutable.Stack.empty[Either[U, U]]
      var c = 0
      for x <- inputUnknowns do stack += Left(x)
      for x <- unknowns do stack += Left(x)

      while !stack.isEmpty do
        stack.pop match
          case Right(u) =>
            dfn += u -> c
            c -= 1
          case Left(u) if !(visited contains u) =>
            visited += u
            stack.push(Right(u))
            for v <- infl(u).reverse do
              if !(visited contains v) then
                dfst += (u -> v)
                stack.push(Left(v))
              else if !dfn.isDefinedAt(v) then heads += v
          case _ =>

    lazy val toSeq: Seq[U] = unknowns.toSeq.sorted(this)

    def compare(x: U, y: U): Int = scala.math.signum(dfn(x) - dfn(y))

    /** Returns whether y is a child of x in the depth-first spanning tree. */
    @tailrec private def connected(x: U, y: U): Boolean =
      dfst.find(_._2 == y) match
        case None    => false
        case Some(z) => if z._1 == x then true else connected(x, z._1)

    def influenceType(x: U, y: U): InfluenceType =
      if y <= x then Retreating
      else if connected(x, y) then Advancing
      else Cross

    def isHead(u: U): Boolean = heads contains u
