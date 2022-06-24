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

import java.util.concurrent.TimeUnit
import java.lang.Math.floorMod

/**
 * This class tests the efficiency of several fixpoint solvers on different
 * variants of the clique equation system with an increment transformation
 * operator. These more closely resemble the form of the equation systems
 * used in the static analysis of programs.
 *
 * @see
 *   [[CliqueEQS]]
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 3)
@Fork(value = 1)
class CliqueBenchIncrement:

  /** Initial assignment. */
  private val initAssignment = Assignment(0)

  /** Number of unknowns. */
  private val numUnknowns = 40

  /** Out-degree of nodes. */
  private val outDegree = 3

  /** Max value used in the widening. */
  private val maxVal = 200

  /** The combo used in the benchmarks. */
  private val combo = Combo[Int]((i: Int, j: Int) => (i max j) min maxVal, true)

  /**
   * Check correctness of the solution of the equation system with standard
   * combo.
   */
  private def validate(rho: Assignment[Int, Int]) =
    assert(
      0 until numUnknowns forall (i =>
        if i < outDegree then rho(i) == maxVal else rho(i) == maxVal + i - outDegree + 1
      )
    )

  /**
   * Check correctness of the solution of the equation system with localized
   * combo.
   */
  private def validate2(rho: Assignment[Int, Int]) =
    var pos = 0
    var curr = maxVal
    val found = scala.collection.mutable.Set.empty[Int]
    while curr < maxVal + numUnknowns do
      assert(rho(pos) == curr)
      curr += 1
      if curr < maxVal + numUnknowns then
        found.add(pos)
        pos = (pos + outDegree) % numUnknowns
        while found contains pos do pos = floorMod(pos - 1, numUnknowns)

  // The equation systems we use for benchmarks
  private val finiteEqs = CliqueEQS.createFiniteEQS[Int](numUnknowns, outDegree, _ + 1)
  private val finiteEqsWithCombos =
    finiteEqs.withCombos(ComboAssignment(combo).restrict(_ < outDegree))
  private val graphEqs = CliqueEQS.createGraphEQS[Int](numUnknowns, outDegree, _ + 1)
  private val graphEqsWithCombos =
    graphEqs.withCombos(ComboAssignment(combo).restrict(_ < outDegree))
  private val ordering = DFOrdering(graphEqs)
  private val graphEqsWithLocalizedCombos =
    graphEqs.withLocalizedCombos(ComboAssignment(combo), ordering)

  // Check correctness of equation solvers
  validate(RoundRobinSolver(finiteEqsWithCombos)(initAssignment))
  validate(RoundRobinSolver(graphEqsWithCombos)(initAssignment))
  validate2(RoundRobinSolver(graphEqsWithLocalizedCombos)(initAssignment))
  validate(WorkListSolver(finiteEqsWithCombos)(initAssignment))
  validate(WorkListSolver(graphEqsWithCombos)(initAssignment))
  validate2(WorkListSolver(graphEqsWithLocalizedCombos)(initAssignment))

  /** Benchmarks the round robin solver using the finite equation system. */
  @Benchmark
  def finiteWithCombos() = RoundRobinSolver(finiteEqsWithCombos)(initAssignment)

  /** Benchmarks the round robin solver using the graph-based equation system */
  @Benchmark
  def graphWithCombos() = RoundRobinSolver(graphEqsWithCombos)(initAssignment)

  /**
   * Benchmarks the round robin solver using the graph-based equation system
   * with localized boxes.
   */
  @Benchmark
  def graphWithLocalizedCombos() = RoundRobinSolver(graphEqsWithLocalizedCombos)(initAssignment)

  /** Benchmarks the worklist solver using the finite equation system. */
  @Benchmark
  def WLfiniteWithCombos() = WorkListSolver(finiteEqsWithCombos)(initAssignment)

  /** Benchmarks the worklist solver using the graph-based equation system. */
  @Benchmark
  def WLgraphWithCombos() = WorkListSolver(graphEqsWithCombos)(initAssignment)

  /**
   * Benchmarks the worklist solver using the graph-based equation system with
   * localized boxes.
   */
  @Benchmark
  def WLgraphWithLocalizedCombos() = WorkListSolver(graphEqsWithLocalizedCombos)(initAssignment)
