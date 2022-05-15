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

package it.unich.scalafix.jmh

import it.unich.scalafix.*
import it.unich.scalafix.assignments.*
import it.unich.scalafix.finite.*
import it.unich.scalafix.graphs.*
import it.unich.scalafix.utils.*

/**
 * This object contains many factory methods which build equation systems made
 * from equations of the form `x(i+1) = x(i)` (here called chain equation
 * system).
 */
object ChainEQS:

  /** Returns a graph based chain equation system with n unknowns. */
  def createGraphEQS[V: Domain](n: Int) = GraphEquationSystem(
    unknowns = 0 until n,
    inputUnknowns = Set(0),
    initialGraph = GraphBody(
      edgeAction = (rho: Assignment[Int, V]) => (i: Int) => rho(i),
      sources = Relation((i: Int) => Set(i)),
      target = (i: Int) => i + 1,
      outgoing = Relation((i: Int) => if i == n - 1 then Set.empty else Set(i)),
      ingoing = Relation((i: Int) => if i == 0 then Set.empty else Set(i - 1)),
      combiner = summon[Domain[V]].upperBound
    )
  )

  /** Returns a finite chain equation system with n unknowns. */
  def createFiniteEQS[V: Domain](n: Int) = FiniteEquationSystem[Int, V](
    unknowns = 0 until n,
    inputUnknowns = Set(0),
    initialBody = (rho: Assignment[Int, V]) => (i: Int) => if i > 0 then rho(i - 1) else rho(0),
    initialInfl = Relation((i: Int) => if i < n - 1 then Set(i + 1) else Set.empty)
  )

  /** Returns an infinite chain equation system. */
  def createInfiniteEQS[V: Domain]() = EquationSystem(
    initialBody = (rho: Assignment[Int, V]) => (i: Int) => if i > 0 then rho(i - 1) else rho(0)
  )

  /**
   * An infinite equation system where `bodyWithDependencies` is provided
   * explicitly.
   *
   * @tparam V
   *   type of the values
   */
  class ChainInfiniteEQS[V] extends BaseEquationSystem[Int, V, ChainInfiniteEQS[V]]:
    override val initialBody = (rho: Assignment[Int, V]) =>
      (i: Int) => if i > 0 then rho(i - 1) else rho(0)
    override val bodyWithDependencies = (rho: Assignment[Int, V]) =>
      (i: Int) => if i > 0 then (rho(i - 1), Seq(i - 1)) else (rho(0), Seq(0))

  /**
   * Returns an infinite chain equation system optimized with a custom
   * `bodyWithDependencies`. Note that calling any of the `withXXX` methods on
   * this equation systems will probably not work as expected.
   */
  def createInfiniteEQSOptimized[V]() = ChainInfiniteEQS[V]()

  /**
   * A graph-based equation system where fields `initialBody` and `initialInfl`
   * are given explicitly.
   *
   * @tparam V
   *   type of the values
   * @param n
   *   number of unknowns
   */
  class ChainGraphEQS[V: Domain](n: Int)
      extends BaseGraphEquationSystem[Int, V, Int, ChainGraphEQS[V]]:
    override val unknowns = 0 until n
    override val inputUnknowns = Set(0)
    override val initialGraph = GraphBody(
      edgeAction = (rho: Assignment[Int, V]) => (i: Int) => rho(i),
      sources = Relation((i: Int) => Set(i)),
      target = (i: Int) => i + 1,
      outgoing = Relation((i: Int) => if i == n - 1 then Set.empty else Set(i)),
      ingoing = Relation((i: Int) => if i == 0 then Set.empty else Set(i - 1)),
      combiner = summon[Domain[V]].upperBound
    )
    override val initialBody = (rho: Assignment[Int, V]) =>
      (i: Int) => if i > 0 then rho(i - 1) else rho(0)
    override val initialInfl = Relation((i: Int) => if i < n - 1 then Set(i + 1) else Set.empty)
    override val bodyWithDependencies: BodyWithDependencies[Int, V] =
      (rho: Assignment[Int, V]) =>
        (i: Int) => if i > 0 then (rho(i - 1), Seq(i - 1)) else (rho(0), Seq.empty)

  /**
   * Returns an finite chain equation system with n unknowns, optimized with a
   * custom implementation of `initialBody`, `initialInfl` and
   * `bodyWithDependencies`. Note that calling any of the `withXXX` methods on
   * this equation systems will probably not work as expected.
   */
  def createGraphEQSOptimized[V: Domain](n: Int) = ChainGraphEQS(n)
