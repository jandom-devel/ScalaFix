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
import it.unich.scalafix.graphs.GraphBodyBuilder.*

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * This class tests the efficiency of several fixpoint solvers on different
 * variants of the clique equation system.
 *
 * @see
 *   [[CliqueEQS]]
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 3)
@Fork(value = 1)
class GraphBuilderBench:
  /** Constant value of the initial assignment. */
  private val initVal = 1

  /** Initial assignment. */
  private val initAssignment = Assignment(0)

  /** Number of unknowns. */
  private val numUnknowns = 20

  /** The combo used in benchmarks. */
  private val combo = Combo[Int](scala.math.max, true)

  /** Check correctness of the solution of the equation system. */
  private def validate1(rho: Assignment[Int, Int]) =
    assert(
      0 until numUnknowns forall (rho(_) == initVal)
    )

  /** Check correctness of the solution of the equation system. */
  private def validate2(rho: Assignment[Node[Int, (Int, Int), Int], Int]) =
    assert(
      graphEqsBuilder.unknowns.forall (rho(_) == initVal)
    )

  /** The clique equation system as a graph-based equation system. */
  private val graphEqs = CliqueEQS
    .createGraphEQS[Int](numUnknowns)
    .withBaseAssignment(Map(0 -> initVal), scala.math.max)

  /** The clique equation system as a builder-based equation system. */
  private var graphEqsBuilder: BaseGraphBuilderEquationSystem[Int, (Int, Int), Int] =
    CliqueEQS.createGraphEQSBuilder[Int](numUnknowns)
  graphEqsBuilder = graphEqsBuilder.withBaseAssignment(Map(graphEqsBuilder.inputUnknowns.toSeq(0) -> initVal), scala.math.max)

  /** The reaching definition equation system as a builder-based equation system. */
  private val reachEqsBuilder = ReachingDefsEQS.createGraphBuilderEQS()

  validate1(RoundRobinSolver(graphEqs)(initAssignment))
  validate2(RoundRobinSolver(graphEqsBuilder)(initAssignment))
  ReachingDefsEQS.validateGraphBuilderAssignment(RoundRobinSolver(reachEqsBuilder)(Assignment(Set())))

  /**
   * Benchmarks the round robin solver using the graph-based equation system.
   *
   * @see
   *   [[CliqueEQS.createGraphEQS]]
   */
  @Benchmark
  def graph() = RoundRobinSolver(graphEqs)(initAssignment)

  @Benchmark
  def graphBuilder() = RoundRobinSolver(graphEqsBuilder)(initAssignment)

  @Benchmark
  def reachBuilder() = RoundRobinSolver(reachEqsBuilder)(Assignment(Set()))
