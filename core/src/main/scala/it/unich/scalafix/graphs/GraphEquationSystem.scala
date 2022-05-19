/**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it> and Francesca
 * Scozzari <francesca.scozzari@unich.it>
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
import it.unich.scalafix.assignments.MapBasedMutableAssignment
import it.unich.scalafix.finite.*
import it.unich.scalafix.utils.Domain
import it.unich.scalafix.utils.Relation

/**
 * A finite equation system generated by an hyper-graph.
 *
 * @tparam U
 *   the type for the unknowns
 * @tparam V
 *   the type for the values assumed by the unknowns.
 * @tparam E
 *   type of edges.
 * @tparam EQS
 *   the type of the equation system. Operations returning a new equation system
 *   generally return `EQS`.
 */
trait GraphEquationSystem[U, V, E, EQS <: GraphEquationSystem[U, V, E, EQS]]
    extends FiniteEquationSystem[U, V, EQS]:

  /** Returns the graph of this equation system. */
  def graph: GraphBody[U, V, E]

  /**
   * Returns the equation system modified with the specified localized combo
   * assignment.
   *
   * @param combos
   *   the combo assignment for the equation system.
   * @param ordering
   *   an ordering on unknowns used to decide the edges where combos need to be
   *   applied.
   * @see
   *   [[GraphBody.addLocalizedCombos]]
   */
  def withLocalizedCombos(
      combos: ComboAssignment[U, V],
      ordering: Ordering[U]
  ): EQS

  /**
   * Returns the equation system modified with the specified combination of
   * localized widening and narrowing (aka localized warrowing).
   * @param widenings
   *   the assignment of widenings to unknowns.
   * @param narrowings
   *   the assignment of narrowings to unknowns.
   * @param ordering
   *   an ordering on unknowns used to decide the edges where combos need to be
   *   applied.
   * @see
   *   [[GraphBody.addLocalizedWarrowing]]
   */
  def withLocalizedWarrowing(
      widenings: ComboAssignment[U, V],
      narrowings: ComboAssignment[U, V],
      ordering: Ordering[U]
  )(using Domain[V]): EQS

/**
 * The base abstract implementation for graph-based equation systems.
 *
 * @tparam U
 *   the type for the unknowns, which are also the nodes of the graph.
 * @tparam V
 *   the type for the values assumed by the unknowns, which is also the type of
 *   the values generated by an edge trough its edge action.
 * @tparam E
 *   type of edges.
 * @tparam EQS
 *   the type of the equation system. Operations returning a new equation system
 *   generally return `EQS`.
 */
abstract class BaseGraphEquationSystem[U, V, E, EQS <: BaseGraphEquationSystem[
  U,
  V,
  E,
  EQS
]] extends BaseFiniteEquationSystem[U, V, EQS]
    with GraphEquationSystem[U, V, E, EQS]
    with Cloneable:

  /**
   * The initial graph of the equation system. Depending on the presence of
   * localized combos, the initial graph is manipulated in order to obtain the
   * real graph.
   */
  protected val initialGraph: GraphBody[U, V, E]

  /**
   * An optional specification of the localized combos we want to apply to the
   * equation system.
   */
  protected var optLocalizedCombos: Option[(ComboAssignment[U, V], Ordering[U])] = None

  /**
   * An optional specification of localized warrowing we want to apply to the
   * equation system.
   */
  protected var optLocalizedWarrowings
      : Option[(ComboAssignment[U, V], ComboAssignment[U, V], Ordering[U], Domain[V])] = None

  /**
   * @inheritdoc
   *
   * @note
   *   The graph is generated starting from the initial graph of the equation
   *   system, eventually adding localized combos as requested by the user.
   */
  override def graph: GraphBody[U, V, E] =
    optLocalizedCombos match
      case Some((localizedCombos, unknownOrdering)) =>
        initialGraph.addLocalizedCombos(localizedCombos, unknownOrdering)
      case _ => initialGraph

  /**
   * @inheritdoc
   *
   * For graph-based equation system, the initial body is computed by the graph.
   */
  override protected def initialBody: Body[U, V] =
    optLocalizedWarrowings match
      case Some((localizedWidenings, localizedNarrowings, unknownOrdering, dom)) =>
        graph.addLocalizedWarrowing(localizedWidenings, localizedNarrowings, unknownOrdering)(
          using dom
        )
      case _ => graph

  /** Returns the body with dependencies of the equations system. */
  override def bodyWithDependencies: BodyWithDependencies[U, V] =
    val body = this.body
    val graph = this.graph
    (rho: Assignment[U, V]) =>
      val bodyRho = body(rho)
      (x: U) => {
        val deps =
          graph
            .ingoing(x)
            .foldLeft(Iterable.empty[U])((acc: Iterable[U], e: E) => acc ++ graph.sources(e))
        val res = bodyRho(x)
        (res, deps)
      }

  /**
   * @inheritdoc
   *
   * For graph-based equation system, the initial influence relation is computed
   * by the graph.
   */
  override def initialInfl: Relation[U, U] =
    val graph = this.graph
    // whey are we using the graph here and not the initial graph ?
    val base = Relation((u: U) => graph.outgoing(u).map(graph.target))
    optLocalizedWarrowings match
      case Some((localizedWidenings, localizedNarrowings, unknownOrdering, dom))
          if !localizedWidenings.combosAreIdempotent || !localizedNarrowings.combosAreIdempotent =>
        base.withDiagonal
      case _ => base

  /** @inheritdoc */
  override def withLocalizedCombos(
      combos: ComboAssignment[U, V],
      ordering: Ordering[U]
  ): EQS =
    val clone = this.clone()
    clone.optLocalizedCombos = Some(combos, ordering)
    clone.optLocalizedWarrowings = None
    clone

  /** @inheritdoc */
  override def withLocalizedWarrowing(
      widenings: ComboAssignment[U, V],
      narrowings: ComboAssignment[U, V],
      ordering: Ordering[U]
  )(using dom: Domain[V]): EQS =
    val clone = this.clone()
    clone.optLocalizedWarrowings = Some(widenings, narrowings, ordering, dom)
    clone.optLocalizedCombos = None
    clone

/**
 * Default implementation of a graph-based equation system.
 *
 * @param initialGraph
 *   the initial graph of the equation system. Depending on the presence of
 *   localized combos, the initial graph is manipulated in order to obtain the
 *   real graph.
 * @param unknowns
 *   collection of all unknowns.
 * @param inputUnknowns
 *   the unknowns which may be considered the input to this equation system.
 */
class SimpleGraphEquationSystem[U, V, E](
    protected val initialGraph: GraphBody[U, V, E],
    val unknowns: Iterable[U],
    val inputUnknowns: Set[U]
) extends BaseGraphEquationSystem[U, V, E, SimpleGraphEquationSystem[U, V, E]]

/** Collection of factory methods for graph-based equation systems. */
object GraphEquationSystem:
  /**
   * Returns the standard implementation of a finite equation system.
   * @see
   *   [[SimpleGraphEquationSystem]] for the meaning of all the parameters.
   */
  def apply[U, V, E](
      initialGraph: GraphBody[U, V, E],
      unknowns: Iterable[U],
      inputUnknowns: Set[U]
  ): SimpleGraphEquationSystem[U, V, E] =
    SimpleGraphEquationSystem(
      initialGraph,
      unknowns,
      inputUnknowns
    )
