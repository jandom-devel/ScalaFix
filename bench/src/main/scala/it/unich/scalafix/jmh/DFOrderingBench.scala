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

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * These are the results of the benchmarks on an Intel Core i5-2400.
 * ```
 * [info] Benchmark                     Mode  Cnt      Score      Error  Units
 * [info] DFOrderingBench.chainFinite   avgt    5    210.092 ±   10.190  us/op
 * [info] DFOrderingBench.chainGraph    avgt    5    209.472 ±    1.986  us/op
 * [info] DFOrderingBench.cliqueFinite  avgt    5  62627.133 ± 3335.524  us/op
 * [info] DFOrderingBench.cliqueGraph   avgt    5  81215.303 ± 3659.380  us/op
 * ```
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 3)
@Fork(value = 1)
class DFOrderingBench {

  /** Number of unknowns. */
  private val numUnknowns = 1000

  private val chainFiniteEqs = ChainEQS.createFiniteEQS[Int](numUnknowns)
  private val chainGraphEqs = ChainEQS.createGraphEQS[Int](numUnknowns)
  private val cliqueGraphEqs = CliqueEQS.createGraphEQS[Int](numUnknowns)
  private val cliqueFiniteEqs = CliqueEQS.createFiniteEQS[Int](numUnknowns)

  @Benchmark
  def chainFinite() = DFOrdering(chainFiniteEqs)

  @Benchmark
  def chainGraph() = DFOrdering(chainGraphEqs)

  @Benchmark
  def cliqueFinite() = DFOrdering(cliqueFiniteEqs)

  @Benchmark
  def cliqueGraph() = DFOrdering(cliqueGraphEqs)
}
