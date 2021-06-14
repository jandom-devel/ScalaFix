/**
  * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.jmh

import it.unich.scalafix.Assignment
import it.unich.scalafix.finite.{RoundRobinSolver, WorkListSolver as FiniteWorkListSolver}
import it.unich.scalafix.infinite.WorkListSolver as InfiniteWorkListSolver
import org.openjdk.jmh.annotations.*

/**
  * This class tests the efficiency of the different fixpoint solvers on
  * different variants of the chain equation system.
  */
@State(Scope.Benchmark)
@Warmup(iterations = 3)
class EquationSystemBench {
  val initVal = 1
  val numUnknowns = 10000
  val chainGraphEqs = new ChainGraphEQS(numUnknowns, initVal)
  val chainSimpleGraphEqs = new ChainSimpleGraphEQS(numUnknowns, initVal)
  val chainSimpleFiniteEqs = new ChainSimpleFiniteEQS(numUnknowns, initVal)
  val chainInfiniteEqs = new ChainInfiniteEQS[Int](initVal)
  val chainInfinite2Eqs = new ChainInfinite2EQS[Int](initVal)

  def validate(rho: Assignment[Int, Int]) = {
    for (i <- 0 until numUnknowns) assert(rho(i) == initVal)
  }

  @Benchmark
  def RRGraph() = {
    val result = RoundRobinSolver(chainGraphEqs)()
    validate(result)
  }

  @Benchmark
  def RRSimpleGraph() = {
    val result = RoundRobinSolver(chainSimpleGraphEqs)()
    validate(result)
  }

  @Benchmark
  def RRSimpleFinite() = {
    val result = RoundRobinSolver(chainSimpleFiniteEqs)()
    validate(result)
  }

  @Benchmark
  def FWLGraph() = {
    val result = FiniteWorkListSolver(chainGraphEqs)()
    validate(result)
  }

  @Benchmark
  def FWLSimpleGraph() = {
    val result = FiniteWorkListSolver(chainSimpleGraphEqs)()
    validate(result)
  }

  @Benchmark
  def FWLSimpleFinite() = {
    val result = FiniteWorkListSolver(chainSimpleFiniteEqs)()
    validate(result)
  }


  @Benchmark
  def IWLInfinite() = {
    val result = InfiniteWorkListSolver(chainInfiniteEqs)(Seq(numUnknowns))
    validate(result)
  }

  @Benchmark
  def IWLInfinite2() = {
    val result = InfiniteWorkListSolver(chainInfinite2Eqs)(Seq(numUnknowns))
    validate(result)
  }
}
