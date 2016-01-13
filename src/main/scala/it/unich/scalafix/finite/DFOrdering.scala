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

import scala.collection.mutable
import scala.annotation.tailrec

/**
  * This trait represents a depth-first ordering of a graph, as it appears in the Aho, Sehti, Ullman book
  * on compilers. It extends the concept of graph ordering distinguishing between Advancing, Retreating
  * and Cross edges.
  *
  * @tparam N the type of the nodes of the graph
  */
abstract class DFOrdering[N] extends GraphOrdering[N] {

  import DFOrdering.EdgeType._

  /**
    * It returns the type of an edge u -> v.
    *
    * @param u sources node
    * @param v target node
    */
  def edgeType(u: N, v: N): EdgeType
}

/**
  * The companion class for a DFOrdering defines the required enumerations and factory
  * methods.
  */
object DFOrdering {

  /**
    * Every edge may be of three different kinds: Advancing, Retreating and Cross.
    */
  object EdgeType extends Enumeration {
    val Advancing = Value
    val Retreating = Value
    val Cross = Value
    type EdgeType = Value
  }

  /**
    * Returns the DFOrdering for a finite equation system.
    */
  def apply[N](eqs: FiniteEquationSystem[N, _]): DFOrdering[N] =
    new DFOrderingFromR[N](eqs.infl, eqs.unknowns, eqs.inputUnknowns)

  /**
    * Returns a DFOrdering for the graph encoded by the influence relation `r`, set of unknowns in `unknowns` and
    * starting nodes in `entries`.
    */
  def apply[N](r: InfluenceRelation[N], unknowns: Iterable[N], entries: Iterable[N]): DFOrdering[N] =
    new DFOrderingFromR[N](r, unknowns, entries)

  /**
    * This class is a depth-first ordering for the influence relation `relation`.
    *
    * @param relation the relation from which we compute the DFOrdering.
    * @param unknowns the set of all unknowns
    * @param entries  nodes from which to start the visit.
    */
  private final class DFOrderingFromR[N](relation: InfluenceRelation[N], unknowns: Iterable[N], entries: Iterable[N]) extends DFOrdering[N] {

    import DFOrdering.EdgeType._

    private val dfn = mutable.HashMap.empty[N, Int]
    private val dfst = mutable.Set.empty[(N, N)]
    private val heads = mutable.Set.empty[N]
    initDFO()

    def initDFO() {
      val visited = mutable.LinkedHashSet.empty[N]
      var c = 0
      for (x <- entries) if (!(visited contains x)) dfsVisit(x)
      for (x <- unknowns) if (!(visited contains x)) dfsVisit(x)

      def dfsVisit(u: N) {
        visited += u
        for (v <- relation(u))
          if (!(visited contains v)) {
            dfst += (u -> v)
            dfsVisit(v)
          } else if (!dfn.isDefinedAt(v)) heads += v
        dfn += u -> c
        c -= 1
      }
    }

    lazy val toSeq = unknowns.toSeq.sorted(this)

    def compare(x: N, y: N) = scala.math.signum(dfn(x) - dfn(y))

    /**
      * Returns whether y is a child of x in the depth-first spanning tree.
      */
    @tailrec private def connected(x: N, y: N): Boolean = {
      val z = dfst.find(_._2 == y)
      if (z.isEmpty)
        false
      else if (z.get._1 == x)
        true
      else
        connected(x, z.get._1)
    }

    def edgeType(x: N, y: N) = if (y <= x)
      Retreating
    else if (connected(x, y))
      Advancing
    else
      Cross

    def isHead(u: N) = heads contains u
  }
}
