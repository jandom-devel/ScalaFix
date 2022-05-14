/**
 * Copyright 2022 Francesca Scozzari <francesca.scozzari@unich.it> and Gianluca
 * Amato <gianluca.amato@unich.it>
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
import it.unich.scalafix.utils.Relation

import org.openjdk.jmh.annotations.*

import scala.collection.mutable

import java.util.concurrent.TimeUnit

/**
 * This benchmark compares the round robin ScalaFix solver with two ad-hoc
 * solvers, one using hash maps for keeping the current assignment, the other
 * using arrays. Differently from other benchmarks, the payload is not
 * completely trivial, therefore we are merely measuring the over-head of
 * ScalaFix, but assessing its impact in a real case.
 */

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime, Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3)
@Fork(value = 1)
class ReachingDefs {

  /** First unknown of the equation system. */
  val firstUnknown = 1

  /** Last unknown of the equation system. */
  val lastUnknown = 7


  /**
   * We consider the following three address code program, with 7 definitions:
   *
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
   *
   * The example comes from: Alfred V. Aho, Ravi Sethi, Jeffrey D. Ullman.
   * Compilers. Principles, Techniques, and Tools Addison-Wesley Publishing
   * Company 1986
   */
  var eqs: SimpleFiniteEquationSystem[Int, Set[Int]] = FiniteEquationSystem(
    initialBody = (rho: Int => Set[Int]) => {
      case 1 => Set(1) -- Set(4, 7)
      case 2 => Set(2) ++ (rho(1) -- Set(5))
      case 3 => Set(3) ++ (rho(2) -- Set())
      case 4 => Set(4) ++ (rho(3) ++ rho(7) ++ rho(6) -- Set(1, 7))
      case 5 => Set(5) ++ (rho(4) -- Set(2))
      case 6 => Set(6) ++ rho(5) -- Set(3)
      case 7 => Set(6) ++ rho(5) -- Set(1, 4)
    },
    initialInfl = Relation(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6, 5 -> 7, 6 -> 4, 7 -> 4),
    inputUnknowns = Set(1),
    unknowns = firstUnknown to lastUnknown
  )

  /** The round robin ScalaFix analyzer. */
  @Benchmark
  def scalafix() = {
    RoundRobinSolver(eqs)(Assignment(Set()))
  }

  /**
   * An ad-hoc static analyzer using hash maps for keeping the current
   * assignment.
   */
  @Benchmark
  def hashMap() = {
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
        if v != vnew then
          rho(i) = vnew
          dirty = true
        i += 1
    rho
  }

  /**
   * An ad-hoc static analyzer using arrays for keeping the current assignment.
   */
  @Benchmark
  def array() = {
    val rho = Array.fill(lastUnknown+1)(Set[Int]())
    var bodyrho = eqs.body(rho)
    var dirty = true
    var i = 1
    while dirty do
      dirty = false
      i = 1
      while i <= lastUnknown do
        val v = rho(i)
        val vnew = bodyrho(i)
        if v != vnew then
          rho(i) = vnew
          dirty = true
        i += 1
    rho
  }

}
