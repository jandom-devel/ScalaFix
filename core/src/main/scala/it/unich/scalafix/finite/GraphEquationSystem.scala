/**
 * Copyright 2015, 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.scalafix.*
import it.unich.scalafix.lattice.{Domain, Magma}
import it.unich.scalafix.utils.Relation

/** The effect of an edge in a graph equation system. */
type EdgeAction[U, V, E] = Assignment[U, V] => E => V

/**
 * This is the abstract class for a finite equation system generated by an
 * hyper-graph. Unknowns are nodes of the graph and each hyper-edge has a single
 * target and many possible sources. Given an assignment, each hyper-edge
 * produces a partial values. These values are combined with the upper bound
 * operation.
 */
trait GraphEquationSystem[U, V, E] extends FiniteEquationSystem[U, V]:
  /** The domain type-class for the type `V`. */
  val dom: Domain[V]

  /**
   * A function which, given an assignment and an edge, returns the output value
   * of the edge.
   */
  val edgeAction: EdgeAction[U, V, E]

  /** Maps each edge to its source unknowns. */
  val sources: E => Iterable[U]

  /** Maps each edge to its target unknown. */
  val target: E => U

  /** Maps each unknown to the collection of edges departing from it. */
  val outgoing: U => Iterable[E]

  /** Maps each unknown to the collection of edges arriving on it. */
  val ingoing: U => Iterable[E]

  /**
   * Add combos to the equation system in a localized way.
   *
   * @param combos
   *   new combos to add.
   * @param ordering
   *   an order on unknown used to decide which edges needs to be widened
   */
  def withLocalizedCombos(
      combos: ComboAssignment[U, V],
      ordering: Ordering[U]
  ): GraphEquationSystem[U, V, E]

  /**
   * Add warrowing to the equation system in a localized way. Localized
   * warrowing requires a different procedure than standard localized widenings.
   * Moreover, it is not entirely clear whether this works as intended or not.
   *
   * @param widenings
   *   a widening assignment
   * @param narrowings
   *   a narrowing assignment
   */
  def withLocalizedWarrowing(
      widenings: ComboAssignment[U, V],
      narrowings: ComboAssignment[U, V],
      ordering: Ordering[U]
  ): FiniteEquationSystem[U, V]

  override def withTracer(t: EquationSystemTracer[U, V]): GraphEquationSystem[U, V, E]

/**
 * A simple standard implementation of FiniteEquationSystem. All fields must be
 * provided explicitly by the user with the exception of `body`,
 * `bodyWithDependencies` and `infl` which are computed by the graph.
 */
case class SimpleGraphEquationSystem[U, V, E](
    unknowns: Iterable[U],
    inputUnknowns: Set[U],
    edgeAction: EdgeAction[U, V, E],
    sources: E => Iterable[U],
    target: E => U,
    outgoing: U => Iterable[E],
    ingoing: U => Iterable[E],
    tracer: Option[EquationSystemTracer[U, V]] = None
)(using val dom: Domain[V])
    extends EquationSystemBase[U, V]
    with GraphEquationSystem[U, V, E]:

  val body: Body[U, V] =
    (rho: Assignment[U, V]) =>
      (x: U) =>
        tracer foreach (_.beforeEvaluation(rho, x))
        val contributions = for e <- ingoing(x) yield edgeAction(rho)(e)
        // if contribution is empty the unknown x has no right hand side... it seems
        // reasonable to return the old value.
        val res =
          if contributions.isEmpty then rho(x)
          else contributions reduce (_ upperBound _)
        tracer foreach (_.afterEvaluation(rho, x, res))
        res

  override val bodyWithDependencies: BodyWithDependencies[U, V] =
    (rho: Assignment[U, V]) =>
      (x: U) => {
        val deps =
          ingoing(x).foldLeft(Iterable.empty[U])((acc: Iterable[U], e: E) => acc ++ sources(e))
        val res = body(rho)(x)
        (res, deps)
      }

  val infl: Relation[U] = Relation({ (u: U) =>
    outgoing(u).view.map(target).to(Set)
  })

  def withTracer(t: EquationSystemTracer[U, V]): GraphEquationSystem[U, V, E] =
    copy(tracer = Some(t))

  def withCombos(combos: ComboAssignment[U, V]): FiniteEquationSystem[U, V] =
    val newbody = bodyWithComboAssignment(combos)
    val newinfl = if combos.combosAreIdempotent then infl else infl.withDiagonal
    SimpleFiniteEquationSystem(newbody, inputUnknowns, unknowns, newinfl, tracer)

  def withBaseAssignment(init: PartialFunction[U, V])(using
      magma: Magma[V]
  ): FiniteEquationSystem[U, V] =
    val newbody = bodyWithBaseAssignment(init, _ op _)
    SimpleFiniteEquationSystem(newbody, inputUnknowns, unknowns, infl, tracer)

  def withLocalizedCombos(
      combos: ComboAssignment[U, V],
      ordering: Ordering[U]
  ): GraphEquationSystem[U, V, E] =
    val newEdgeAction =
      (rho: Assignment[U, V]) =>
        (e: E) =>
          val x = target(e)
          if combos.isDefinedAt(x) && sources(e).exists(ordering.lteq(x, _)) then
            combos(x)(rho(x), edgeAction(rho)(e))
          else edgeAction(rho)(e)
    if combos.combosAreIdempotent then copy(edgeAction = newEdgeAction)
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

  def withLocalizedWarrowing(
      widenings: ComboAssignment[U, V],
      narrowings: ComboAssignment[U, V],
      ordering: Ordering[U]
  ): FiniteEquationSystem[U, V] =
    val newBody: Body[U, V] =
      (rho: Assignment[U, V]) =>
        (x: U) =>
          val contributions = for e <- ingoing(x) yield
            val contrib = edgeAction(rho)(e)
            val comboapply = sources(e).exists(ordering.lteq(x, _)) && !dom.lteq(contrib, rho(x))
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
    val newInfl =
      if widenings.combosAreIdempotent && narrowings.combosAreIdempotent then infl
      else infl.withDiagonal
    SimpleFiniteEquationSystem(newBody, inputUnknowns, unknowns, newInfl)

object GraphEquationSystem:
  /**
   * Returns the standard implementation of GraphEquationSystem. All fields must
   * be provided explicitly by the user with the exception of `body`,
   * `bodyWithDependencies` and `infl`.
   */
  def apply[U, V: Domain, E](
      unknowns: Iterable[U],
      inputUnknowns: Set[U],
      edgeAction: EdgeAction[U, V, E],
      source: E => Iterable[U],
      target: E => U,
      outgoing: U => Iterable[E],
      ingoing: U => Iterable[E]
  ): GraphEquationSystem[U, V, E] =
    SimpleGraphEquationSystem(
      unknowns,
      inputUnknowns,
      edgeAction,
      source,
      target,
      outgoing,
      ingoing,
      None
    )
