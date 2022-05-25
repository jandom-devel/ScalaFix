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
import it.unich.scalafix.finite.*
import it.unich.scalafix.finite.WorkListSolver as FiniteWorkListSolver
import it.unich.scalafix.infinite.WorkListSolver as InfiniteWorkListSolver
import it.unich.scalafix.utils.*

import org.openjdk.jmh.annotations.*

/**
 * This class tests the efficiency of several fixpoint solvers on different
 * variants of the chain equation system.
 *
 * These are the results of the benchmarks on an Intel Core i5-2500K.
 * ```
 * [info] Benchmark                          Mode  Cnt     Score    Error  Units
 * [info] Chain.FWLFinite                   thrpt    5  1089.263 ± 15.523  ops/s
 * [info] Chain.FWLGraph                    thrpt    5   789.915 ±  2.185  ops/s
 * [info] Chain.FWLGraphOptimized           thrpt    5  1111.895 ±  3.244  ops/s
 * [info] Chain.IWLInfinite                 thrpt    5   739.240 ±  2.131  ops/s
 * [info] Chain.IWLInfiniteOptimized        thrpt    5   795.049 ± 11.447  ops/s
 * [info] Chain.RRFinite                    thrpt    5  5513.842 ± 11.899  ops/s
 * [info] Chain.RRFiniteWithCombos          thrpt    5  1849.733 ±  8.432  ops/s
 * [info] Chain.RRGraph                     thrpt    5  1901.863 ±  7.061  ops/s
 * [info] Chain.RRGraphWithCombos           thrpt    5  1674.762 ±  6.008  ops/s
 * [info] Chain.RRGraphWithLocalizedCombos  thrpt    5  1127.178 ±  3.856  ops/s
 * ```
 *
 * @see
 *   [[ChainEQS]]
 */
@State(Scope.Thread)
@Warmup(iterations = 3)
@Fork(value = 1)
class Chain:
  /** Constant value of the initial assignment. */
  private val initVal = 1

  /** Initial assignment. */
  private val initAssignment = Assignment(initVal)

  /** Number of unknowns. */
  private val numUnknowns = 10000

  /** The combo used in benchmarks. */
  private val combo = Combo.right[Int]

  private val chainGraphEqsOptimized = ChainEQS.createGraphEQSOptimized[Int](numUnknowns)
  private val chainGraphEqs = ChainEQS.createGraphEQS[Int](numUnknowns)
  private val chainGraphWithCombosEqs = chainGraphEqs.withCombos(ComboAssignment(combo))
  private val chainFiniteEqs = ChainEQS.createFiniteEQS[Int](numUnknowns)
  private val chainFiniteWithCombosEqs = chainGraphEqs.withCombos(ComboAssignment(combo))
  private val chainInfiniteEqsOptimized = ChainEQS.createInfiniteEQSOptimized[Int]()
  private val chainInfiniteEqs = ChainEQS.createInfiniteEQS[Int]()

  private var ordering = DFOrdering(chainGraphEqs)
  private val chainGraphWithLocalizedCombosEqs =
    chainGraphEqs.withLocalizedCombos(ComboAssignment(combo), ordering)

  /** Check correctness of the solution of the equation system. */
  private def validate(rho: Assignment[Int, Int]): Unit =
    for i <- 0 until numUnknowns do assert(rho(i) == initVal)

  validate(RoundRobinSolver(chainGraphEqsOptimized)(initAssignment))
  validate(RoundRobinSolver(chainGraphEqs)(initAssignment))
  validate(RoundRobinSolver(chainGraphWithCombosEqs)(initAssignment))
  validate(RoundRobinSolver(chainFiniteEqs)(initAssignment))
  validate(RoundRobinSolver(chainFiniteWithCombosEqs)(initAssignment))
  validate(RoundRobinSolver(chainGraphWithLocalizedCombosEqs)(initAssignment))
  validate(InfiniteWorkListSolver(chainInfiniteEqsOptimized)(initAssignment, Seq(numUnknowns)))
  validate(InfiniteWorkListSolver(chainInfiniteEqs)(initAssignment, Seq(numUnknowns)))

  /**
   * Benchmarks the round robin solver on the optimized version of the
   * graph-based chain equation system.
   *
   * @see
   *   [[ChainEQS.createGraphEQSOptimized]]
   */
  def RRGraphOptimized() =
    val result = RoundRobinSolver(chainGraphEqsOptimized)(initAssignment)

  /**
   * Benchmarks the round robin solver on the standard graph-based chain
   * equation system.
   *
   * @see
   *   [[ChainEQS.createGraphEQS]]
   */
  @Benchmark
  def RRGraph() = RoundRobinSolver(chainGraphEqs)(initAssignment)

  /**
   * Benchmarks the round robin solver on the standard graph-based chain
   * equation system with with an additional max combo at each unknown.
   *
   * @see
   *   [[ChainEQS.createGraphEQS]]
   */
  @Benchmark
  def RRGraphWithCombos() = RoundRobinSolver(chainGraphWithCombosEqs)(initAssignment)

  /**
   * Benchmarks the round robin solver on the standard graph-based chain
   * equation system with with an additional max localized combo at each
   * unknown.
   *
   * @see
   *   [[ChainEQS.createGraphEQS]]
   */
  @Benchmark
  def RRGraphWithLocalizedCombos() =
    RoundRobinSolver(chainGraphWithLocalizedCombosEqs)(initAssignment)

  /**
   * Benchmarks the round robin solver on the finite chain equation system.
   *
   * @see
   *   [[ChainEQS.createFiniteEQS]]
   */
  @Benchmark
  def RRFinite() = RoundRobinSolver(chainFiniteEqs)(initAssignment)

  /**
   * Benchmarks the round robin solver on the finite chain equation system, with
   * an additional max combo at each unknown.
   *
   * @see
   *   [[ChainEQS.createFiniteEQS]]
   */
  @Benchmark
  def RRFiniteWithCombos() = RoundRobinSolver(chainFiniteWithCombosEqs)(initAssignment)

  /**
   * Benchmarks the worklist solver on the optimized version of the graph-based
   * chain equation system.
   *
   * @see
   *   [[ChainEQS.createGraphEQSOptimized]]
   */
  @Benchmark
  def FWLGraphOptimized() = FiniteWorkListSolver(chainGraphEqsOptimized)(initAssignment)

  /**
   * Benchmarks the worklist solver on the standard graph-based chain equation
   * system.
   *
   * @see
   *   [[ChainEQS.createGraphEQS]]
   */
  @Benchmark
  def FWLGraph() = FiniteWorkListSolver(chainGraphEqs)(initAssignment)

  /**
   * Benchmarks the worklist solver on the finite chain equation system.
   *
   * @see
   *   [[ChainEQS.createFiniteEQS]]
   */
  @Benchmark
  def FWLFinite() = FiniteWorkListSolver(chainFiniteEqs)(initAssignment)

  /**
   * Benchmarks the worklist solver on the optimized version of the infinite
   * equation system.
   *
   * @see
   *   [[ChainEQS.createInfiniteEQSOptimized]]
   */
  @Benchmark
  def IWLInfiniteOptimized() =
    InfiniteWorkListSolver(chainInfiniteEqsOptimized)(initAssignment, Seq(numUnknowns))

  /**
   * Benchmarks the worklist solver on standard infinite equation system.
   *
   * @see
   *   [[ChainEQS.createInfiniteEQS]]
   */
  @Benchmark
  def IWLInfinite() = InfiniteWorkListSolver(chainInfiniteEqs)(initAssignment, Seq(numUnknowns))
