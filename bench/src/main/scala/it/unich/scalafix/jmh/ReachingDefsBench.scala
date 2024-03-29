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
import it.unich.scalafix.finite.*

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
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 3)
@Fork(value = 1)
class ReachingDefsBench:

  val eqs = ReachingDefsEQS.createFiniteEQS()

  /** Combo used in the equation systems. */
  private val combo = Combo[Set[Int]](_ ++ _, true)

  /** The finite equationm system with combos. */
  private val eqsCombo = eqs.withCombos(ComboAssignment(combo))

  val graphEqs = ReachingDefsEQS.createGraphEQS()

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
      while i <= ReachingDefsEQS.lastUnknown do
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
    val rho = Array.fill(ReachingDefsEQS.lastUnknown + 1)(Set[Int]())
    var bodyrho = eqs.body(rho)
    var dirty = true
    var i = 1
    while dirty do
      dirty = false
      i = 1
      while i <= ReachingDefsEQS.lastUnknown do
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
