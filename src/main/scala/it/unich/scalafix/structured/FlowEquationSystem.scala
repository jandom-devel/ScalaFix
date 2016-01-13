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

package it.unich.scalafix.structured

import it.unich.scalafix._
import it.unich.scalafix.finite._
import it.unich.scalafix.utils.Relation
import it.unich.scalafix.lattice.Magma

/**
 * This is the trait for a layered equation system where each layer contain a single equation of the
 * type `x_i = f(x_j)`. In other words, each edge has one sources and one target. This kind of
 * equation systems always arise for dataflow equations. Note, however, that it does not
 * seem to have any performance advantage w.r.t. the general case, and it is less expressive.
 * @tparam U the type for the unknowns of this equation system.
 * @tparam V the type for the values assumed by the unknowns of this equation system.
 * @tparam E the type of edges of this equaton system
 */
abstract class FlowEquationSystem[U, V: Magma, E] extends GraphBasedEquationSystem[U, V, E] {
  /**
   * It return a function which, given an assignment and edge, returns the output value of
   * the edge.
   */
  def edgeAction: (U => V) => E => V

  /**
   * Maps each edge to its sources unknown.
   */
  def source: E => U

  /**
   * Maps each edge to its target unknown.
   */
  def target: E => U

  /**
   * Maps each unknown to the collection of edges departing from it.
   */
  def outgoing: U => Iterable[E]

  /**
   * Maps each unknown to the collection of edges arriving on it.
   */
  def ingoing: U => Iterable[E]
}

object FlowEquationSystem {
  import EquationSystem._

  /**
   * An alias for the type of edgeAction.
   */
  type EdgeAction[U, V, E] = (U => V) => E => V

  /**
   * Returns an implementation of `FlowEquationSystem` from a subset of its constituents.
   */
  def apply[U, V: Magma, E](unknowns: Iterable[U], inputUnknowns: Iterable[U], edgeAction: EdgeAction[U, V, E],
    source: E => U, target: E => U, outgoing: U => Iterable[E], ingoing: U => Iterable[E], initial: U => V) =
    SimpleFlowEquationSystem(unknowns, inputUnknowns, edgeAction, source, target, outgoing, ingoing, initial)

  /**
   * An implementation of `FlowEquationSystem` from a subset of its constituents.
   */
  final case class SimpleFlowEquationSystem[U, V, E](
    val unknowns: Iterable[U],
    val inputUnknowns: Iterable[U],
    val edgeAction: EdgeAction[U, V, E],
    val source: E => U,
    val target: E => U,
    val outgoing: U => Iterable[E],
    val ingoing: U => Iterable[E],
    val initial: U => V)(implicit magma: Magma[V])
      extends FlowEquationSystem[U, V, E] with FiniteEquationSystem.WithBaseAssignment[U, V] {

    private lazy val edges: Set[E] = (for (u <- unknowns; e <- outgoing(u)) yield e)(collection.breakOut)

    lazy val sources = Relation(edges, { e: E => Set(source(e)) })

    lazy val targets = Relation(edges, { e: E => Set(target(e)) })

    def edgeBody = (e: E) => (rho: U => V) => (x: U) => edgeAction(rho)(e)

    val body = new Body[U,V] {
      def apply(rho: Assignment[U,V]) = {
        (x: U) =>
          val contributions = for (e <- ingoing(x)) yield edgeAction(rho)(e)
          // if contribution is empty the unknown x has no right hand side... it seems
          // reasonable to return the old value.
          if (contributions.isEmpty) rho(x) else contributions reduce { magma.op }
      }
    }

    def bodyWithDependencies(rho: Assignment[U,V])(x: U) = {
        val deps = for (e <- ingoing(x)) yield source(e)
        val res = body(rho)(x)
        (res, deps)
      }

    val infl: InfluenceRelation[U] = InfluenceRelation({ (u: U) => (for (e <- outgoing(u)) yield target(e))(collection.breakOut) } )

    def withBaseAssignment(init: PartialFunction[U, V]) = FiniteEquationSystem(
      body = body.withBaseAssignment(init),
      unknowns = unknowns,
      inputUnknowns = inputUnknowns,
      infl = infl)

    def withBoxes(boxes: BoxAssignment[U, V]) = FiniteEquationSystem(
      body = body.withBoxAssignment(boxes),
      inputUnknowns = inputUnknowns,
      unknowns = unknowns,
      infl = if (boxes.areIdempotent) infl else infl.withDiagonal
    )

    def withLocalizedBoxes(boxes: BoxAssignment[U, V], ordering: Ordering[U]) = {
      if (boxes.areIdempotent) {
        val newEdgeAction: EdgeAction[U, V, E] = { (rho: U => V) =>
          e: E =>
            val x = target(e)
            if (boxes.isDefinedAt(x) && ordering.lteq(x, source(e))) {
              boxes(x)(rho(x), edgeAction(rho)(e))
            } else
              edgeAction(rho)(e)
        }
        copy(edgeAction = newEdgeAction)
      } else {
        val asLayered = GraphBasedEquationSystem(unknowns, inputUnknowns, edgeBody, sources, targets, initial)
        asLayered.withLocalizedBoxes(boxes, ordering)
      }
    }
  }
}
