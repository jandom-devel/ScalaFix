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

import it.unich.scalafix.*
import it.unich.scalafix.assignments.defaultMutableAssignmentFactory
import it.unich.scalafix.finite.{RoundRobinSolver, WorkListSolver as FiniteWorkListSolver}
import it.unich.scalafix.infinite.WorkListSolver as InfiniteWorkListSolver
import it.unich.scalafix.lattice.given

import org.openjdk.jmh.annotations.*

/**
  * This class tests the efficiency of the different fixpoint solvers on
  * different variants of the chain equation system.
  */
@State(Scope.Benchmark)
@Warmup(iterations = 3)
class EquationSystemBench:
  val initVal = 1
  val initAssignment = Assignment(initVal)
  val numUnknowns = 10000
  val chainGraphEqs = new ChainGraphEQS[Int](numUnknowns)
  val chainSimpleGraphEqs = new ChainSimpleGraphEQS[Int](numUnknowns)
  val chainSimpleFiniteEqs = new ChainSimpleFiniteEQS[Int](numUnknowns)
  val chainInfiniteEqs = new ChainInfiniteEQS[Int]()
  val chainInfinite2Eqs = new ChainInfinite2EQS[Int]()

  def validate(rho: Assignment[Int, Int]) =
    for i <- 0 until numUnknowns do assert(rho(i) == initVal)

  @Benchmark
  def RRGraph() =
    val result = RoundRobinSolver(chainGraphEqs)(initAssignment)
    validate(result)

  @Benchmark
  def RRSimpleGraph() =
    val result = RoundRobinSolver(chainSimpleGraphEqs)(initAssignment)
    validate(result)

  @Benchmark
  def RRSimpleFinite() =
    val result = RoundRobinSolver(chainSimpleFiniteEqs)(initAssignment)
    validate(result)

  @Benchmark
  def FWLGraph() =
    val result = FiniteWorkListSolver(chainGraphEqs)(initAssignment)
    validate(result)

  @Benchmark
  def FWLSimpleGraph() =
    val result = FiniteWorkListSolver(chainSimpleGraphEqs)(initAssignment)
    validate(result)

  @Benchmark
  def FWLSimpleFinite() =
    val result = FiniteWorkListSolver(chainSimpleFiniteEqs)(initAssignment)
    validate(result)


  @Benchmark
  def IWLInfinite() =
    val result = InfiniteWorkListSolver(chainInfiniteEqs)(initAssignment, Seq(numUnknowns))
    validate(result)

  @Benchmark
  def IWLInfinite2() =
    val result = InfiniteWorkListSolver(chainInfinite2Eqs)(initAssignment, Seq(numUnknowns))
    validate(result)
