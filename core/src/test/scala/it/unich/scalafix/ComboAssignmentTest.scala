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

package it.unich.scalafix

import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ComboAssignmentTest extends AnyFunSpec with ScalaCheckPropertyChecks:

  private val maxcombo = Combo { (x: Int, y: Int) => x max y }

  def checkDefinedEverywhere[V](combos: ComboAssignment[Int, V]): Unit =
    it("is defined everywhere") {
      forAll { (u: Int) => assertResult(true)(combos.isDefinedAt(u)) }
    }

  def checkIsEmpty(combos: ComboAssignment[Nothing, ?]): Unit =
    it("is empty") {
      assertResult(true)(combos.isEmpty)
    }

  def checkIsNotEmpty(combos: ComboAssignment[Nothing, ?]): Unit =
    it("is not empty") {
      assertResult(false)(combos.isEmpty)
    }

  def checkIsIdempotent(combos: ComboAssignment[Nothing, ?]): Unit =
    it("is idempotent") {
      assertResult(true)(combos.combosAreIdempotent)
    }

  def checkIsNotIdempotent(combos: ComboAssignment[Nothing, ?]): Unit =
    it("is not idempotent") {
      assertResult(false)(combos.combosAreIdempotent)
    }

  def copyIdempotent(combos: ComboAssignment[Nothing, ?]): Unit =
    it("has a copy method which returns itself") {
      assertResult(combos)(combos.copy)
    }

  def copyNotIdempotent(combos: ComboAssignment[Nothing, ?]): Unit =
    it("has a copy method which returns a new object") {
      assert(combos != combos.copy)
    }

  describe("The empty combo assignment") {
    val combos = ComboAssignment.empty[Int]
    it("it returns a right combo for each unknown") {
      forAll { (u: Int) => assertResult(true)(combos(u).isRight) }
    }
    it("is defined nowhere") {
      forAll { (u: Int) => assertResult(true)(!combos.isDefinedAt(u)) }
    }
    checkIsEmpty(combos)
    checkIsIdempotent(combos)
    copyIdempotent(combos)
  }

  describe("The constant combo assignment for the min combo") {
    val combos: ComboAssignment[Int, Int] = ComboAssignment { (x: Int, y: Int) => x min y }
    it("returns the same combo for each unknown") {
      forAll { (u: Int) => assertResult(true)(combos(u) eq combos(u)) }
    }
    it("returns the correct combo") {
      forAll { (u: Int, x: Int, y: Int) =>
        assertResult(x min y)(combos(u)(x, y))
      }
    }
    checkIsNotEmpty(combos)
    checkDefinedEverywhere(combos)
    checkIsIdempotent(combos)
    copyIdempotent(combos)
  }

  describe("The combo assignment built from an immutable idempotent combo") {
    val combos = ComboAssignment(maxcombo)
    it("returns the same combo for each unknown") {
      forAll { (u: Int) => assertResult(true)(maxcombo eq combos(u)) }
    }
    it("returns the same combo each time it is applied to the same unknown") {
      forAll { (u: Int) => assertResult(true)(combos(u) eq combos(u)) }
    }
    checkIsNotEmpty(combos)
    checkDefinedEverywhere(combos)
    checkIsIdempotent(combos)
    copyIdempotent(combos)
  }

  describe("The combo assignment built from a mutable combo") {
    val maxcomboDelayed = maxcombo.delayed(2)
    val combos = ComboAssignment(maxcomboDelayed)
    it("returns a different combo for each unknown") {
      forAll { (u: Int) =>
        assertResult(false)(maxcomboDelayed eq combos(u))
        forAll { (v: Int) =>
          whenever(u != v) {
            assertResult(false)(combos(v) eq combos(u))
          }
        }
      }
    }
    it("returns the same combo each time it is applied to the same unknown") {
      forAll { (u: Int) => assertResult(true)(combos(u) eq combos(u)) }
    }
    checkIsNotEmpty(combos)
    checkDefinedEverywhere(combos)
    checkIsNotIdempotent(combos)
    copyNotIdempotent(combos)
  }
