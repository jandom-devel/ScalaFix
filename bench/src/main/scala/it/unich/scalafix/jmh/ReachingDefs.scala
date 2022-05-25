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

package it.unich.scalafix.jmh

import it.unich.scalafix.*
import it.unich.scalafix.assignments.*
import it.unich.scalafix.finite.*
import it.unich.scalafix.graphs.*
import it.unich.scalafix.utils.Relation

import scala.collection.mutable

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * This benchmark compares different round robin solvers, either based on
 * ScalaFix or written ad hoc. Differently from other benchmarks, the payload is
 * not completely trivial, therefore we are not merely measuring the over-head
 * of ScalaFix, but assessing its impact in a real case.
 *
 * In particular, the equation system is the one used to perform a reaching
 * definition analisysis on the folling three-address code program:
 * ```
 * d1 --> i = m-1;
 * d2 --> j = n;
 * d3 --> a = u1;
 * do
 *   d4 --> i = i+1;
 *   d5 --> j = j-1;
 *   if (e1) then
 *     d6 --> a = u2;
 *   else
 *     d7 --> i = u3
 * while (e2)
 * ```
 * The example comes from: <blockquote> Alfred V. Aho, Ravi Sethi, Jeffrey D.
 * Ullman.<br> <em>Compilers. Principles, Techniques, and Tools</em><br>
 * Addison-Wesley Publishing Company, 1986 </blockquote>
 *
 * These are the results of the benchmarks on an Intel Core i5-2500K.
 * ```
 * [info] Benchmark                                       Mode  Cnt      Score     Error  Units
 * [info] ReachingDefs.array                             thrpt    5  35286.065 ± 117.418  ops/s
 * [info] ReachingDefs.arrayWithCombos                   thrpt    5  22822.220 ± 130.105  ops/s
 * [info] ReachingDefs.hashMap                           thrpt    5  34208.490 ±  60.324  ops/s
 * [info] ReachingDefs.hashMapWithCombos                 thrpt    5  22031.984 ± 202.610  ops/s
 * [info] ReachingDefs.scalafix                          thrpt    5  34324.632 ± 137.073  ops/s
 * [info] ReachingDefs.scalafixGraph                     thrpt    5  28995.099 ± 722.278  ops/s
 * [info] ReachingDefs.scalafixGraphWithCombos           thrpt    5  18214.238 ±  39.789  ops/s
 * [info] ReachingDefs.scalafixGraphWithLocalizedCombos  thrpt    5  25451.629 ± 437.353  ops/s
 * [info] ReachingDefs.scalafixWithCombos                thrpt    5  21671.204 ± 531.168  ops/s
 * ```
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 3)
@Fork(value = 1)
class ReachingDefs:

  /** First unknown of the equation system. */
  private val firstUnknown = 1

  /** Last unknown of the equation system. */
  private val lastUnknown = 7

  /** The finite equation system. */
  private val eqs = FiniteEquationSystem[Int, Set[Int]](
    initialBody = (rho: Int => Set[Int]) => {
      case 1 => Set(1) -- Set(4, 7)
      case 2 => Set(2) ++ (rho(1) -- Set(5))
      case 3 => Set(3) ++ (rho(2) -- Set(7))
      case 4 => Set(4) ++ (rho(3) ++ rho(7) ++ rho(6) -- Set(1, 7))
      case 5 => Set(5) ++ (rho(4) -- Set(2))
      case 6 => Set(6) ++ (rho(5) -- Set(3))
      case 7 => Set(7) ++ (rho(5) -- Set(1, 4))
    },
    initialInfl = Relation(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6, 5 -> 7, 6 -> 4, 7 -> 4),
    unknowns = firstUnknown to lastUnknown,
    inputUnknowns = Set(1)
  )

  /** Combo used in the equation systems. */
  private val combo = Combo[Set[Int]](_ ++ _, true)

  /** The finite equationm system with combos. */
  private val eqsCombo = eqs.withCombos(ComboAssignment(combo))

  /** The graph based body of the equation system. */
  private val graphBody = GraphBody[(Int, Boolean), Set[Int], String](
    sources = Relation(
      "1to2" -> (1, false),
      "2to3" -> (2, false),
      "3to4'" -> (3, false),
      "6to4'" -> (3, false),
      "7to4'" -> (3, false),
      "4'to4" -> (4, true),
      "4to5" -> (4, false),
      "5to6" -> (5, false),
      "5to7" -> (5, false)
    ),
    target = Map(
      "*to1" -> (1, false),
      "1to2" -> (2, false),
      "2to3" -> (3, false),
      "3to4'" -> (4, true),
      "6to4'" -> (4, true),
      "7to4'" -> (4, true),
      "4'to4" -> (4, false),
      "4to5" -> (5, false),
      "5to6" -> (6, false),
      "5to7" -> (7, false)
    ),
    ingoing = Relation(
      (1, false) -> "*to1",
      (2, false) -> "1to2",
      (3, false) -> "2to3",
      (4, true) -> "3to4'",
      (4, true) -> "6to4'",
      (4, true) -> "7to4'",
      (4, false) -> "4'to4",
      (5, false) -> "4to5",
      (6, false) -> "5to6",
      (7, false) -> "5to7"
    ),
    outgoing = Relation(
      (1, false) -> "1to2",
      (2, false) -> "2to3",
      (3, false) -> "3to4'",
      (4, true) -> "4'to4",
      (4, false) -> "4to5",
      (5, false) -> "5to6",
      (5, false) -> "5to7",
      (6, false) -> "6to4'",
      (7, false) -> "7to4'"
    ),
    edgeAction = (rho: Assignment[(Int, Boolean), Set[Int]]) => {
      case "*to1"  => Set(1) -- Set(4, 7)
      case "1to2"  => Set(2) ++ (rho(1, false) -- Set(5))
      case "2to3"  => Set(3) ++ (rho(2, false) -- Set(6))
      case "3to4'" => rho(3, false)
      case "6to4'" => rho(6, false)
      case "7to4'" => rho(7, false)
      case "4'to4" => Set(4) ++ (rho(4, true) -- Set(1, 7))
      case "4to5"  => Set(5) ++ (rho(4, false) -- Set(2))
      case "5to6"  => Set(6) ++ (rho(5, false) -- Set(3))
      case "5to7"  => Set(7) ++ (rho(5, false) -- Set(1, 4))
    },
    combiner = _ ++ _
  )

  /** The graph-based version of the equation system. */
  private val graphEqs = GraphEquationSystem(
    initialGraph = graphBody,
    unknowns = ((firstUnknown to lastUnknown) map { (i: Int) => (i, false) }) ++ Seq((4, true)),
    inputUnknowns = Set((1, false))
  )

  /** The graph-based version of the equation system wth combos. */
  private val graphEqsCombo = graphEqs.withCombos(ComboAssignment(combo))

  /** The DF ordering for the graph-based equation system. */
  private val ordering = DFOrdering(graphEqs)

  /** The graph-based version of the equation system wth localized combos. */
  private val graphEqsLocalizedCombos =
    graphEqs.withLocalizedCombos(ComboAssignment(combo), ordering)

  /** Validate the correcteness of a solution. */
  private def validate(rho: Assignment[Int, Set[Int]]) =
    assert(rho(1) == Set(1))
    assert(rho(2) == Set(1, 2))
    assert(rho(3) == Set(1, 2, 3))
    assert(rho(4) == Set(2, 3, 4, 5, 6))
    assert(rho(5) == Set(3, 4, 5, 6))
    assert(rho(6) == Set(4, 5, 6))
    assert(rho(7) == Set(3, 5, 6, 7))

  validate(RoundRobinSolver(eqs)(Assignment(Set())))
  validate(RoundRobinSolver(eqsCombo)(Assignment(Set())))
  validate {
    val rho = RoundRobinSolver(graphEqs)(Assignment(Set()))
    (i: Int) => rho(i, false)
  }
  validate {
    val rho = RoundRobinSolver(graphEqsCombo)(Assignment(Set()))
    (i: Int) => rho(i, false)
  }
  validate {
    val rho = RoundRobinSolver(graphEqsLocalizedCombos)(Assignment(Set()))
    (i: Int) => rho(i, false)
  }

  /**
   * Solve the clique equation system with a custom round-robin solver, using an
   * hash map for keeping the current assignment.
   *
   * @param withCombos
   *   if true, an additional set-union combo is added to all the unknowns.
   */
  private def hashMapSolve(withCombos: Boolean) =
    val rho = mutable.Map.empty[Int, Set[Int]].withDefaultValue(Set())
    var bodyrho = eqs.body(rho)
    var dirty = true
    var i = 1
    while dirty do
      dirty = false
      i = 1
      while i <= lastUnknown do
        val v = rho(i)
        val vnew = bodyrho(i)
        val vres = if withCombos then vnew ++ vnew else vnew
        if v != vnew then
          rho(i) = vnew
          dirty = true
        i += 1
    rho

  /**
   * Solve the clique equation system with a custom round-robin solver, using an
   * array for keeping the current assignment.
   *
   * @param withCombos
   *   if true, an additional set-union combo is added to all the unknowns.
   */
  private def arraySolve(withWidening: Boolean) =
    val rho = Array.fill(lastUnknown + 1)(Set[Int]())
    var bodyrho = eqs.body(rho)
    var dirty = true
    var i = 1
    while dirty do
      dirty = false
      i = 1
      while i <= lastUnknown do
        val v = rho(i)
        val vnew = bodyrho(i)
        val vres = if withWidening then vnew ++ vnew else vnew
        if v != vres then
          rho(i) = vres
          dirty = true
        i += 1
    rho

  /**
   * Benchmarks the round robin ScalaFix analyzer using the finite equation
   * system.
   */
  @Benchmark
  def scalafix() = RoundRobinSolver(eqs)(Assignment(Set()))

  /**
   * Benchmarks the round robin ScalaFix analyzer using the finite equation
   * system with an additional set-union combo at each unknown.
   */
  @Benchmark
  def scalafixWithCombos() = RoundRobinSolver(eqsCombo)(Assignment(Set()))

  /**
   * Benchmarks the round robin ScalaFix analyzer using the graph based equation
   * system.
   */
  @Benchmark
  def scalafixGraph() = RoundRobinSolver(graphEqs)(Assignment(Set()))

  /**
   * Benchmarks the round robin ScalaFix analyzer using the graph based equation
   * system with additional set-union combo at each unknown.
   */
  @Benchmark
  def scalafixGraphWithCombos() = RoundRobinSolver(graphEqsCombo)(Assignment(Set()))

  /**
   * Benchmarks the round robin ScalaFix analyzer using the graph based equation
   * system with additional localized set-union combo at each unknown.
   */
  @Benchmark
  def scalafixGraphWithLocalizedCombos() =
    RoundRobinSolver(graphEqsLocalizedCombos)(Assignment(Set()))

  /**
   * Benchmarks an ad-hoc static analyzer using hash maps for keeping the
   * current assignment.
   */
  @Benchmark
  def hashMap() = hashMapSolve(false)

  /**
   * Benchmarks an ad-hoc static analyzer using hash maps for keeping the
   * current assignment. It includes an addition set-union combo at each
   * unknowns.
   */
  @Benchmark
  def hashMapWithCombos() = hashMapSolve(true)

  /**
   * Benchmarks an ad-hoc static analyzer using arrays for keeping the current
   * assignment.
   */
  @Benchmark
  def array() = arraySolve(false)

  /**
   * Benchmarks an ad-hoc static analyzer using arrays for keeping the current
   * assignment. It includes an addition set-union combo at each unknowns.
   */
  @Benchmark
  def arrayWithCombos() = arraySolve(true)
