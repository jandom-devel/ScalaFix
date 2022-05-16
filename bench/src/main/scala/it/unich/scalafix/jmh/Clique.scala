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
 * [info] Benchmark                           Mode  Cnt    Score   Error  Units
 * [info] Clique.WLfinite                    thrpt    5  183.012 ± 2.249  ops/s
 * [info] Clique.WLfiniteWithCombos          thrpt    5  222.433 ± 6.117  ops/s
 * [info] Clique.WLgraph                     thrpt    5   87.586 ± 3.201  ops/s
 * [info] Clique.WLgraphWithCombos           thrpt    5   84.117 ± 0.352  ops/s
 * [info] Clique.WLgraphWithLocalizedCombos  thrpt    5   20.344 ± 0.065  ops/s
 * [info] Clique.finite                      thrpt    5  190.622 ± 1.858  ops/s
 * [info] Clique.finiteWithCombos            thrpt    5  190.462 ± 2.078  ops/s
 * [info] Clique.graph                       thrpt    5   85.498 ± 0.730  ops/s
 * [info] Clique.graphWithCombos             thrpt    5   86.506 ± 0.529  ops/s
 * [info] Clique.graphWithLocalizedCombos    thrpt    5   19.188 ± 0.071  ops/s
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

  private val finiteEqs = CliqueEQS.createFiniteEQS[Int](numUnknowns)
  private val finiteEqsWithCombos = finiteEqs.withCombos(ComboAssignment(combo))
  private val graphEqs = CliqueEQS.createGraphEQS[Int](numUnknowns)
  private val graphEqsWithCombos = graphEqs.withCombos(ComboAssignment(combo))
  private val ordering = DFOrdering(graphEqs)
  private val graphEqsWithLocalizedCombos =
    graphEqs.withLocalizedCombos(ComboAssignment(combo), ordering)

  validate(RoundRobinSolver(finiteEqs)(initAssignment))
  validate(RoundRobinSolver(finiteEqsWithCombos)(initAssignment))
  validate(RoundRobinSolver(graphEqs)(initAssignment))
  validate(RoundRobinSolver(graphEqsWithCombos)(initAssignment))
  validate(RoundRobinSolver(graphEqsWithLocalizedCombos)(initAssignment))
  validate(WorkListSolver(finiteEqs)(initAssignment))
  validate(WorkListSolver(finiteEqsWithCombos)(initAssignment))
  validate(WorkListSolver(graphEqs)(initAssignment))
  validate(WorkListSolver(graphEqsWithCombos)(initAssignment))
  validate(WorkListSolver(graphEqsWithLocalizedCombos)(initAssignment))

  /**
   * Benchmarks the round robin solver using the finite equation system.
   *
   * @see
   *   [[CliqueEQS.createFiniteEQS]]
   */
  @Benchmark
  def finite() = RoundRobinSolver(finiteEqs)(initAssignment)

  /**
   * Benchmarks the round robin solver using the finite equation system with an
   * additional max combo at each unknown.
   *
   * @see
   *   [[CliqueEQS.createFiniteEQS]]
   */
  @Benchmark
  def finiteWithCombos() = RoundRobinSolver(finiteEqsWithCombos)(initAssignment)

  /**
   * Benchmarks the round robin solver using the graph-based equation system.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]
   */
  @Benchmark
  def graph() = RoundRobinSolver(graphEqs)(initAssignment)

  /**
   * Benchmarks the round robin solver using the graph-based equation system
   * with an additional max combo at each unknown.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]
   */
  @Benchmark
  def graphWithCombos() = RoundRobinSolver(graphEqsWithCombos)(initAssignment)

  /**
   * Benchmarks the round robin solver using the graph-based equation system
   * with an additional max combo at each unknown.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]
   */
  @Benchmark
  def graphWithLocalizedCombos() = RoundRobinSolver(graphEqsWithLocalizedCombos)(initAssignment)

  /**
   * Benchmarks the worklist solver using the finite equation system.
   *
   * @see
   *   [[CliqueEQS.createFiniteEQS]]
   */
  @Benchmark
  def WLfinite() = WorkListSolver(finiteEqs)(initAssignment)

  /**
   * Benchmarks the worklist solver using the finite equation system with an
   * additional max combo at each unknown.
   *
   * @see
   *   [[CliqueEQS.createFiniteEQS]]
   */
  @Benchmark
  def WLfiniteWithCombos() = WorkListSolver(finiteEqsWithCombos)(initAssignment)

  /**
   * Benchmarks the worklist solver using the graph-based equation system.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]
   */
  @Benchmark
  def WLgraph() = WorkListSolver(graphEqs)(initAssignment)

  /**
   * Benchmarks the worklist solver using the graph-based equation system with
   * an additional max combo at each unknown.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]
   */
  @Benchmark
  def WLgraphWithCombos() = WorkListSolver(graphEqsWithCombos)(initAssignment)

  /**
   * Benchmarks the worklist solver using the graph-based equation system with
   * an additional max combo at each unknown.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]
   */
  @Benchmark
  def WLgraphWithLocalizedCombos() = WorkListSolver(graphEqsWithLocalizedCombos)(initAssignment)
