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

import org.openjdk.jmh.annotations.*

/**
 * This class tests the efficiency of several fixpoint solvers on different
 * variants of the clique equation system.
 *
 * These are the results of the benchmarks on an Intel Core i5-2500K.
 * ```
 * [info] Benchmark                         Mode  Cnt    Score   Error  Units
 * [info] Clique.finite                    thrpt    5  325.181 ± 4.953  ops/s
 * [info] Clique.finiteWithCombos          thrpt    5  320.771 ± 3.109  ops/s
 * [info] Clique.graph                     thrpt    5   13.971 ± 0.670  ops/s
 * [info] Clique.graphWithCombos           thrpt    5   14.030 ± 0.056  ops/s
 * [info] Clique.graphWithLocalizedCombos  thrpt    5    6.457 ± 0.022  ops/s
 * ```
 *
 * @see
 *   [[CliqueEQS]]
 */
@State(Scope.Benchmark)
@Warmup(iterations = 3)
@Fork(value = 1)
class Clique:
  /** Constant value of the initial assignment. */
  private val initVal = 1

  /** Initial assignment. */
  private val initAssignment = Assignment(initVal)

  /** Number of unknowns. */
  private val numUnknowns = 500

  /** The combo used in benchmarks. */
  private val combo = Combo[Int](scala.math.max, true)

  /** Check correctness of the solution of the equation system. */
  private def validate(rho: Assignment[Int, Int]) = assert(
    (0 until numUnknowns).forall(i => rho(i) == initVal)
  )

  private val graphEqs = CliqueEQS.createGraphEQS[Int](numUnknowns)
  private val finiteEqs = CliqueEQS.createFiniteEQS[Int](numUnknowns)

  validate(RoundRobinSolver(graphEqs)(initAssignment))
  validate(RoundRobinSolver(finiteEqs)(initAssignment))
  validate(RoundRobinSolver(finiteEqs.withCombos(ComboAssignment(combo)))(initAssignment))

  /**
   * Benchmarks the round robin ScalaFix analyzer using the finite equation
   * system.
   *
   * @see
   *   [[CliqueEQS.createFiniteEQS]]*
   */
  @Benchmark
  def finite() =
    RoundRobinSolver(finiteEqs)(initAssignment)

  /**
   * Benchmarks the round robin ScalaFix analyzer using the finite equation
   * system with an additional max combo at each unknown.
   *
   * @see
   *   [[CliqueEQS.createFiniteEQS]]*
   */
  @Benchmark
  def finiteWithCombos() =
    val eqs = finiteEqs.withCombos(ComboAssignment(combo))
    RoundRobinSolver(eqs)(initAssignment)

  /**
   * Benchmarks the round robin ScalaFix analyzer using the graph-based equation
   * system.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]*
   */
  @Benchmark
  def graph() =
    RoundRobinSolver(graphEqs)(initAssignment)

  /**
   * Benchmarks the round robin ScalaFix analyzer using the graph-based equation
   * system with an additional max combo at each unknown.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]*
   */
  @Benchmark
  def graphWithCombos() =
    val eqs = graphEqs.withCombos(ComboAssignment(combo))
    RoundRobinSolver(eqs)(initAssignment)

  /**
   * Benchmarks the round robin ScalaFix analyzer using the graph-based equation
   * system with an additional max combo at each unknown.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]*
   */
  @Benchmark
  def graphWithLocalizedCombos() =
    val ordering = DFOrdering(finiteEqs)
    val eqs = graphEqs.withLocalizedCombos(ComboAssignment(combo), ordering)
    RoundRobinSolver(eqs)(initAssignment)
