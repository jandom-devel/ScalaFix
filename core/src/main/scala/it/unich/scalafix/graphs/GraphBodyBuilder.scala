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

package it.unich.scalafix.graphs

import it.unich.scalafix.*
import it.unich.scalafix.utils.*

import scala.collection.mutable

/**
 * This class may be used to build a [[GraphBody]] using an imperativeprogramming style.
 * Moreover, it introduce mutable assignments implemented directly as fields of nodes,
 * which should be faster than hashmap based assignments.
 *
 * @tparam LN
 *   type of labels for nodes
 * @tparam LE
 *   type of labels for edges
 * @tparam V
 *   the type for the values assumed by the unknowns
 */
open class GraphBodyBuilder[LN, LE, V]:
  import GraphBodyBuilder.*

  /** Type of nodes (unknowns) of the generated graph body. */
  type U = Node[LN, LE, V]

  /** Type of edges of the generated graph body. */
  type E = Edge[LN, LE, V]

  /**
   * A mutable assignment for equation system built with [[GraphBodyBuilder]].
   *
   * This mutable assignment is implemented by endowing each node with an
   * `assignments` field, which is an [[ArrayBuffer]] of elements of type `V`.
   * Each index of the array corresponds to a different assignment. A global
   * counter keep track of the next available index which may be allocated by a
   * `GraphMutableAssignment`.
   */
  class GraphMutableAssignment(rho: Assignment[U, V]) extends MutableAssignment[U, V]:
    private var idx = lastIdx
    lastIdx += 1
    for u <- GraphBodyBuilder.this.unknowns do u.assignments.addOne(None)

    override def unknowns =
      GraphBodyBuilder.this.unknowns.view filter (_.assignments(idx).isDefined)

    override def isDefinedAt(u: U) = u.assignments(idx).isDefined

    override def update(u: U, v: V) = u.assignments(idx) = Some(v)

    override def apply(u: U) = u.assignments(idx).getOrElse(rho(u))

  /** The counter for the next available index for the mutable assignments. */
  private var lastIdx = 0

  /** List of unknowns (nodes) of the graph body under construction. */
  private val unknowns = mutable.ListBuffer.empty[U]

  /**
   * Adds a node to the graph.
   *
   * @param label
   *   the label of the node.
   * @return
   *   the added node.
   */
  def addNode(label: LN): U =
    val u = Node[LN, LE, V](label)
    unknowns.addOne(u)
    u

  /**
   * Adds an (hyper-)edge to the graph.
   *
   * @param label
   *   the label of the edge.
   * @param sources
   *   the sequences of source nodes of the edge.
   * @param target
   *   the target node of the edge.
   * @param action
   *   the action of this edge (see [[EdgeAction]]).
   * @return
   *   the added edge.
   */
  def addEdge(label: LE, sources: Seq[U], target: U, action: Assignment[U, V] => V): E =
    val e = Edge[LN, LE, V](label, target, sources, action)
    target.ingoing += e
    sources foreach (_.outgoing += e)
    e

  /** Returns the graph body built by this `GraphBodyBuilder`. */
  def toGraphBody(combiner: (V, V) => V): GraphBody[U, V, E] = GraphBody(
    sources = Relation((e: E) => e.sources),
    target = (e: E) => e.target,
    ingoing = Relation((n: U) => n.ingoing),
    outgoing = Relation((n: U) => n.outgoing),
    edgeAction = (rho: Assignment[U, V]) => (e: E) => e.action(rho),
    unknowns,
    combiner
  )

/** Auxiliary classes and factory methods for the [[GraphBodyBuilder]] class. */
object GraphBodyBuilder:

  /** Nodes (unknown) for the graph built by [[GraphBodyBuilder]]. */
  class Node[LN, LE, V](val label: LN):
    val ingoing = mutable.ListBuffer.empty[Edge[LN, LE, V]]
    val outgoing = mutable.ListBuffer.empty[Edge[LN, LE, V]]
    val assignments = mutable.ArrayBuffer.empty[Option[V]]
    override def toString = label.toString

  /** Edges for the graph built by [[GraphBodyBuilder]]. */
  class Edge[LN, LE, V](
      val label: LE,
      val target: Node[LN, LE, V],
      val sources: Seq[Node[LN, LE, V]],
      val action: Assignment[Node[LN, LE, V], V] => V
  ):
    override def toString = label.toString

  /**
   * Returns an empty graph body builder.
   *
   * @tparam LN
   *   type of labels for nodes.
   * @tparam LE
   *   type of labels for edges.
   * @tparam V
   *   the type for the values assumed by the unknowns.
   */
  def apply[LN, LE, V]() = new GraphBodyBuilder[LN, LE, V]()
