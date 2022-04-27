/**
 * Copyright 2015, 2016, 2017, 2022 Gianluca Amato <gianluca.amato@unich.it>
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
import it.unich.scalafix.lattice.Domain

/** The effect of an edge in a graph equation system. */
type EdgeAction[U, V, E] = Assignment[U, V] => E => V

/**
 * An hyper graph, i.e., a graph where each edge as several sources nodes and a
 * single target node,
 *
 * @tparam U
 *   type of nodes.
 * @tparam E
 *   type of edges.
 * @param sources
 *   maps each edge to its source unknowns.
 * @param target
 *   each edge to its target unknown.
 * @param ingoing
 *   maps each unknown to the collection of edges departing from it.
 * @param outgoing
 *   maps each unknown to the collection of edges arriving on it.
 */
case class Graph[U, V, E](
    sources: E => Iterable[U],
    target: E => U,
    outgoing: U => Iterable[E],
    ingoing: U => Iterable[E],
    edgeAction: EdgeAction[U, V, E]
)(using dom: Domain[V]) extends Body[U, V]:

  def apply(rho: Assignment[U, V]) = (x: U) =>
    val contributions = for e <- ingoing(x) yield edgeAction(rho)(e)
    // if contribution is empty the unknown x has no right hand side... it seems
    // reasonable to return the old value.
    if contributions.isEmpty then rho(x) else contributions reduce (_ upperBound _)

  def addLocalizedCombos(
      combos: ComboAssignment[U, V],
      ordering: Ordering[U]
  ): Graph[U, V, E] =
    val newEdgeAction =
      (rho: Assignment[U, V]) =>
        (e: E) =>
          val x = target(e)
          if combos.isDefinedAt(x) && sources(e).exists(ordering.lteq(x, _)) then
            combos(x)(rho(x), edgeAction(rho)(e))
          else edgeAction(rho)(e)
    if combos.combosAreIdempotent
    then copy(edgeAction = newEdgeAction)
    else
      val newSources =
        (e: E) =>
          val x = target(e)
          if combos.isDefinedAt(x) && sources(e).exists(ordering.lteq(x, _)) then
            sources(e) ++ Iterable(x)
          else sources(e)
      val newOutgoing =
        (u: U) =>
          if combos.isDefinedAt(u) then
            val edges = ingoing(u).filter { (e: E) =>
              sources(e).exists(ordering.lteq(u, _))
            }
            outgoing(u) ++ edges
          else outgoing(u)
      copy(edgeAction = newEdgeAction, sources = newSources, outgoing = newOutgoing)

  def addLocalizedWarrowing(
      widenings: ComboAssignment[U, V],
      narrowings: ComboAssignment[U, V],
      ordering: Ordering[U]
  ): Body[U, V] =
      (rho: Assignment[U, V]) =>
        (x: U) =>
          val contributions = for e <- ingoing(x) yield
            val contrib = edgeAction(rho)(e)
            val comboapply = sources(e).exists(ordering.lteq(x, _)) && ! dom.lteq(contrib, rho(x))
            (contrib, comboapply)
          // if contribution is empty the unknown x has no right hand side... it seems
          // reasonable to return the old value.
          if contributions.isEmpty then rho(x)
          else
            val result = contributions reduce { (x: (V, Boolean), y: (V, Boolean)) =>
              (x._1 upperBound y._1, x._2 || y._2)
            }
            if result._2 then widenings(x)(rho(x), result._1)
            else if dom.lt(result._1, rho(x)) then narrowings(x)(rho(x), result._1)
            else result._1
