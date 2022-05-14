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

package it.unich.scalafix.finite

import it.unich.scalafix.*
import it.unich.scalafix.assignments.*
import it.unich.scalafix.utils.Relation

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Test solvers for finite equation systems. */
class FiniteEquationSystemTest extends AnyFunSpec with ScalaCheckPropertyChecks:

  import HierarchicalOrdering.*

  private val simpleEqs = FiniteEquationSystem[Int, Double](
    initialBody = { (rho: Int => Double) =>
      {
        case 0 => rho(0)
        case 1 => (rho(0) max rho(2)) min rho(3)
        case 2 => rho(1) + 1
        case 3 => rho(3)
      }
    },
    initialInfl = Relation(0 -> Set(0, 1, 2), 1 -> Set(2), 2 -> Set(1), 3 -> Set(1, 3)),
    unknowns = 0 to 3,
    inputUnknowns = Set(0)
  )

  private val simpleEqsStrategy =
    HierarchicalOrdering(Left, Val(0), Left, Val(1), Val(2), Val(3), Right, Right)
  private val wideningCombo = ComboAssignment { (x1: Double, x2: Double) =>
    if x2 > x1 then Double.PositiveInfinity else x1
  }
  private val maxCombo = ComboAssignment { (x: Double, y: Double) => x max y }
  private val lastCombo = ComboAssignment { (_: Double, x2: Double) => x2 }
  private val startRho = Assignment(0.0).updated(3, 10.0)

  private type SimpleSolver[U, V] =
    (FiniteEquationSystem[U, V, ?], Assignment[U, V]) => MutableAssignment[U, V]

  /**
   * Tests whether solving `eqs` equation system always returns a correct
   * result. Should be used only for solvers which are guaranteed to terminate
   * with the given equation system.
   */
  def testCorrectness[U, V](
      eqs: FiniteEquationSystem[U, V, ?],
      solver: SimpleSolver[U, V]
  )(using
      values: Arbitrary[V]
  ) =
    val startRhosList = Gen.listOfN(eqs.unknowns.size, values.arbitrary)
    val startRhos = startRhosList map { l => Map.from(eqs.unknowns.toList zip l) }
    forAll(startRhos) { start =>
      val finalEnv = solver(eqs, Assignment(start))
      for x <- eqs.unknowns do assert(finalEnv(x) === eqs.body(finalEnv)(x))
    }

  /**
   * Test solvers for the `simpleEqs` equation system when starting from the
   * initial assignment `startRho`.
   */
  def testExpectedResult(solver: SimpleSolver[Int, Double]) =
    it("gives the expected result starting from startRho with last") {
      val finalRho = solver(simpleEqs.withCombos(lastCombo), startRho)
      assert(finalRho(0) === 0.0)
      assert(finalRho(1) === 10.0)
      assert(finalRho(2) === 11.0)
      assert(finalRho(3) === 10.0)
    }

    it("gives the expected result starting from startRho with max") {
      val finalRho = solver(simpleEqs.withCombos(maxCombo), startRho)
      assert(finalRho(0) === 0.0)
      assert(finalRho(1) === 10.0)
      assert(finalRho(2) === 11.0)
      assert(finalRho(3) === 10.0)
    }

    it("gives the expected result starting from startRho with widenings") {
      val finalRho = solver(simpleEqs.withCombos(wideningCombo), startRho)
      assert(finalRho(0) === 0.0)
      assert(finalRho(1) === Double.PositiveInfinity)
      assert(finalRho(2) === Double.PositiveInfinity)
      assert(finalRho(3) === 10.0)
    }

    it("always returns a combo solution with widenings") {
      testCorrectness(simpleEqs.withCombos(wideningCombo), solver)
    }

  describe("The RoundRobinSolver") {
    testExpectedResult(RoundRobinSolver(_)(_))
  }
  describe("The WorkListSolver") {
    testExpectedResult(WorkListSolver(_)(_))
  }
  describe("The KleeneSolver") {
    testExpectedResult(KleeneSolver(_)(_))
  }
  describe("The PriorityWorkListSolver") {
    testExpectedResult(PriorityWorkListSolver(_)(_, simpleEqsStrategy))
  }
  describe("The HierarchicalOrderingSolver") {
    testExpectedResult(HierarchicalOrderingSolver(_)(_, simpleEqsStrategy))
  }
