/**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it> and
 *                       Francesca Scozzari <francesca.scozzari@unich.it>
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

package it.unich.scalafix.infinite

import it.unich.scalafix.*
import it.unich.scalafix.assignments.*

import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.mutable
import it.unich.scalafix.infinite.PriorityWorkListSolver.DynamicPriority

/** Test solvers for finite equation systems. */
class InfiniteEquationSystemTest extends AnyFunSpec with ScalaCheckPropertyChecks:

  private val simpleEqs = EquationSystem[Int, Int](
    initialBody = { (rho: Int => Int) => (x: Int) =>
      if x % 2 == 0 then rho(rho(x)) max x / 2
      else {
        val n = (x - 1) / 2
        rho(6 * n + 4)
      }
    }
  )

  private val maxCombo = ComboAssignment { (x: Int, y: Int) => x max y }
  private val startRho = Assignment(0)

  private type SimpleSolver[U, V] =
    (EquationSystem[U, V, ?], Assignment[U, V], Seq[U]) => MutableAssignment[U, V]

  class EvaluationOrderListener[U, V] extends FixpointSolverTracerAdapter[U, V]:
    private val buffer = mutable.Buffer.empty[Any]

    override def evaluated(rho: Assignment[U, V], x: U, newval: V) =
      buffer += x
      ()

  /**
   * Test solvers for the `simpleEqs` equation system when starting from the
   * initial assignment `startRho`.
   */
  def testExpectedResult(solver: SimpleSolver[Int, Int]) =
    it("gives the expected result starting from startRho with max") {
      val finalRho = solver(simpleEqs.withCombos(maxCombo), startRho, Seq(4))
      assertResult(Set(0, 1, 2, 4))(finalRho.unknowns)
      assertResult(2)(finalRho(1))
      assertResult(2)(finalRho(2))
      assertResult(2)(finalRho(4))
    }

  describe("The standard bodyWithDependencies method") {
    it("returns the correct dependencies") {
      assertResult((2, Seq(4, 0))) {
        simpleEqs.bodyWithDependencies(startRho)(4)
      }
      assertResult((0, Seq(4))) {
        simpleEqs.bodyWithDependencies(startRho)(1)
      }
    }
    it("returns the same value as body") {
      forAll { (x: Int) =>
        assertResult(simpleEqs.body(startRho)(x)) {
          simpleEqs.bodyWithDependencies(startRho)(x)._1
        }
      }
    }
  }

  describe("The DynamicPriorityOrdering") {
    it("puts new element first in the ordering") {
      val o = new PriorityWorkListSolver.DynamicPriority[Int]
      o.lteq(1, 1)
      assert(o.lt(2, 1))
      assert(o.lt(3, 2))
      assert(o.lt(3, 1))
      assert(o.lt(-10, 2))
    }
  }

  describe("The WorkListSolver") {
    testExpectedResult(WorkListSolver(_)(_, _))

     it ("does not throw exceptions when the wanted unknowns are immediately updated") {
      val eqs = EquationSystem({(rho: Assignment[Int, Int]) => (x: Int) => 1 })
      val rho = Assignment(0)
      val finalRho = WorkListSolver(eqs)(rho,Seq(0))
      assertResult(Set(0))(finalRho.unknowns)
      assertResult(1)(finalRho(0))
    }
  }

  describe("The PriorityWorkListSolver") {
    testExpectedResult(PriorityWorkListSolver(_)(_, _))

     it ("does not throw exceptions when the wanted unknowns are immediately updated") {
      val eqs = EquationSystem({(rho: Assignment[Int, Int]) => (x: Int) => 1 })
      val rho = Assignment(0)
      val finalRho = PriorityWorkListSolver(eqs)(rho,Seq(0))
      assertResult(Set(0))(finalRho.unknowns)
      assertResult(1)(finalRho(0))
    }
  }
