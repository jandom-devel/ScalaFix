/**
 * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.scalafix.lattice.given

import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ComboTest extends AnyFunSpec with ScalaCheckPropertyChecks:

  import scala.math.Ordering.given

  private val intWidening = Combo { (x: Int, y: Int) => if x >= y then x else Int.MaxValue }
  private val intNarrowing = Combo { (x: Int, y: Int) => if x == Int.MaxValue then y else x }
  private val intMax = Combo { (x: Int, y: Int) => x max y }
  private val intUpperBound = Combo.upperBound[Int]

  def testIsNotRight[V](combo: Combo[V]): Unit =
    it("is not a right combo") {
      assertResult(false)(combo.isRight)
    }

  def testIsRight[V](combo: Combo[V]): Unit =
    it("is a right combo") {
      assertResult(true)(combo.isRight)
    }

  def testImmutable[V](combo: Combo[V]): Unit =
    it("is declared immutable") {
      assertResult(true)(combo.isImmutable)
    }
    it("returns the same combo each time copy is called") {
      assertResult(true)(combo eq combo.copy)
    }

  def testMutable[V](combo: Combo[V]): Unit =
    it("is declared mutable") {
      assertResult(false)(combo.isImmutable)
    }
    it("returns different combos each time copy is called") {
      assertResult(false)(combo eq combo.copy)
    }

  describe("A left combo") {
    val combo = Combo.left[Int]
    it("returns the first element") {
      forAll { (x: Int, y: Int) => assertResult(x)(combo(x, y)) }
    }
    testIsNotRight(combo)
    testImmutable(combo)
  }

  describe("A right combo") {
    val combo = Combo.right[Int]
    it("returns the second element") {
      forAll { (x: Int, y: Int) => assertResult(y)(combo(x, y)) }
    }
    testIsRight(combo)
    testImmutable(combo)
  }

  describe("The combo obtained from the max function using fromFunction") {
    val combo = intMax
    it("returns the maximum element") {
      forAll { (x: Int, y: Int) => assertResult(x max y)(combo(x, y)) }
    }
    testIsNotRight(combo)
    testImmutable(combo)
  }

  describe("The combo obtained from the upper bound of integer ordering") {
    val combo = intUpperBound
    it("returns the maximum element") {
      forAll { (x: Int, y: Int) => assertResult(x max y)(combo(x, y)) }
    }
    testIsNotRight(combo)
    testImmutable(combo)
  }

  describe(
    "The warrowing obtained by combining standard widenings and narrowings on a directed partial ordering"
  ) {
    val combo = Combo.warrowing(intWidening, intNarrowing)
    it("returns the expected results") {
      assertResult(4)(combo(combo(3, 5), 4))
      assertResult(3)(combo(combo(3, 2), 1))
    }
    testIsNotRight(combo)
    testImmutable(combo)
  }

  describe("The warrowing obtained by two right combos") {
    val combo = Combo.warrowing(Combo.right[Int], Combo.right[Int])
    testIsRight(combo)
    testImmutable(combo)
  }

  describe("The combo obtained by cascading") {
    it("generate an exception if delay is negative") {
      intercept[IllegalArgumentException](Combo.cascade(Combo.right[Int], -3, intMax))
    }
    describe("when combining two right combos") {
      val combo = Combo.cascade(Combo.right[Int], 2, Combo.right)
      testIsRight(combo)
      testImmutable(combo)
    }
    describe("when combining a right combo with a max combo with positive delay") {
      val combo = Combo.cascade(Combo.right[Int], 2, intMax)
      it("behaves as a right combo for delay steps") {
        assertResult(2)(combo(3, 2))
        assertResult(2)(combo(3, 2))
      }
      it("behaves as a max combo later on") {
        assertResult(3)(combo(3, 2))
        assertResult(3)(combo(3, 2))

      }
      testIsNotRight(combo)
      testMutable(combo)
    }
    describe("when combining a right combo with a max combo with null delay") {
      val combo = Combo.cascade(Combo.right[Int], 0, intMax)
      it("behaves as the max combo") {
        assertResult(3)(combo(3, 2))
        assertResult(3)(combo(3, 2))
      }
      testIsNotRight(combo)
      testImmutable(combo)
    }
    describe("when the second combo is a right combo and delay is zero") {
      val combo = Combo.cascade(intMax, 0, Combo.right[Int])
      testIsRight(combo)
      testImmutable(combo)
    }
  }

  describe("Delaying the max combo") {
    it("generate an exception if delay is negative") {
      intercept[IllegalArgumentException](intMax.delayed(-3))
    }
    describe("when combining a right combo with a max combo with positive delay") {
      val combo = intMax.delayed(2)
      it("behaves as a right combo for delay steps") {
        assertResult(2)(combo(3, 2))
        assertResult(2)(combo(3, 2))
      }
      it("behaves as a max combo later on") {
        assertResult(3)(combo(3, 2))
        assertResult(3)(combo(3, 2))

      }
      testIsNotRight(combo)
      testMutable(combo)
    }
    describe("when combining a right combo with a max combo with null delay") {
      val combo = intMax.delayed(0)
      it("behaves as the max combo") {
        assertResult(3)(combo(3, 2))
        assertResult(3)(combo(3, 2))
      }
      testIsNotRight(combo)
      testImmutable(combo)
    }
  }

  describe("A delayed right combo") {
    val combo = Combo.right[Int].delayed(2)
    testIsRight(combo)
    testImmutable(combo)
  }

  describe("A warrowing built from a delayed widenings") {
    val combo = Combo.warrowing(intMax.delayed(2), Combo.left[Int])
    testIsNotRight(combo)
    testMutable(combo)
  }
