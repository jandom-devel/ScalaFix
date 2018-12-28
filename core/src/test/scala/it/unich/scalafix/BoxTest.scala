/**
  * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks

class BoxTest extends FunSpec with PropertyChecks {
  implicit val IntIsPartialOrdering: PartialOrdering[Int] = implicitly[Ordering[Int]]

  val intWidening: Box[Int] = { (x: Int, y: Int) => if (x >= y) x else Int.MaxValue }
  val intNarrowing: Box[Int] = { (x: Int, y: Int) => if (x == Int.MaxValue) y else x }
  val intMax: Box[Int] = { (x: Int, y: Int) => x max y }
  val intUpperBound: Box[Int] = Box.upperBound[Int]

  def testIsNotRight[V](box: Box[V]): Unit = {
    it("is not a right box") {
      assertResult(false)(box.isRight)
    }
  }

  def testIsRight[V](box: Box[V]): Unit = {
    it("is a right box") {
      assertResult(true)(box.isRight)
    }
  }

  def testImmutable[V](box: Box[V]): Unit = {
    it("is declared immutable") {
      assertResult(true)(box.isImmutable)
    }
    it("returns the same box each time copy is called") {
      assertResult(true)(box eq box.copy)
    }
  }

  def testMutable[V](box: Box[V]): Unit = {
    it("is declared mutable") {
      assertResult(false)(box.isImmutable)
    }
    it("returns different boxes each time copy is called") {
      assertResult(false)(box eq box.copy)
    }
  }

  describe("A left box") {
    val box = Box.left[Int]
    it("returns the first element") {
      forAll { (x: Int, y: Int) =>
        assertResult(x)(box(x, y))
      }
    }
    testIsNotRight(box)
    testImmutable(box)
  }

  describe("A right box") {
    val box = Box.right[Int]
    it("returns the second element") {
      forAll { (x: Int, y: Int) =>
        assertResult(y)(box(x, y))
      }
    }
    testIsRight(box)
    testImmutable(box)
  }

  describe("The box obtained from the max function using fromFunction") {
    val box = intMax
    it("returns the maximum element") {
      forAll { (x: Int, y: Int) =>
        assertResult(x max y)(box(x, y))
      }
    }
    testIsNotRight(box)
    testImmutable(box)
  }

  describe("The box obtained from the upper bound of integer ordering") {
    val box = intUpperBound
    it("returns the maximum element") {
      forAll { (x: Int, y: Int) =>
        assertResult(x max y)(box(x, y))
      }
    }
    testIsNotRight(box)
    testImmutable(box)
  }

  describe("The warrowing obtained by combining standard widenings and narrowings on a directed partial ordering") {
    val box = Box.warrowing(intWidening, intNarrowing)
    it("returns the expected results") {
      assertResult(4)(box(box(3, 5), 4))
      assertResult(3)(box(box(3, 2), 1))
    }
    testIsNotRight(box)
    testImmutable(box)
  }

  describe("The warrowing obtained by two right boxes") {
    val box = Box.warrowing(Box.right[Int], Box.right[Int])
    testIsRight(box)
    testImmutable(box)
  }

  describe("The box obtained by cascading") {
    it("generate an exception if delay is negative") {
      intercept[IllegalArgumentException](Box.cascade(Box.right[Int], -3, intMax))
    }
    describe("when combining two right boxes") {
      val box = Box.cascade(Box.right[Int], 2, Box.right)
      testIsRight(box)
      testImmutable(box)
    }
    describe("when combining a right box with a max box with positive delay") {
      val box = Box.cascade(Box.right[Int], 2, intMax)
      it("behaves as a right box for delay steps") {
        assertResult(2)(box(3, 2))
        assertResult(2)(box(3, 2))
      }
      it("behaves as a max box later on") {
        assertResult(3)(box(3, 2))
        assertResult(3)(box(3, 2))

      }
      testIsNotRight(box)
      testMutable(box)
    }
    describe("when combining a right box with a max box with null delay") {
      val box = Box.cascade(Box.right[Int], 0, intMax)
      it("behaves as the max box") {
        assertResult(3)(box(3, 2))
        assertResult(3)(box(3, 2))
      }
      testIsNotRight(box)
      testImmutable(box)
    }
    describe("when the second box is a right box and delay is zero") {
      val box = Box.cascade(intMax, 0, Box.right[Int])
      testIsRight(box)
      testImmutable(box)
    }
  }

  describe("Delaying the max box") {
    it("generate an exception if delay is negative") {
      intercept[IllegalArgumentException](intMax.delayed(-3))
    }
    describe("when combining a right box with a max box with positive delay") {
      val box = intMax.delayed(2)
      it("behaves as a right box for delay steps") {
        assertResult(2)(box(3, 2))
        assertResult(2)(box(3, 2))
      }
      it("behaves as a max box later on") {
        assertResult(3)(box(3, 2))
        assertResult(3)(box(3, 2))

      }
      testIsNotRight(box)
      testMutable(box)
    }
    describe("when combining a right box with a max box with null delay") {
      val box = intMax.delayed(0)
      it("behaves as the max box") {
        assertResult(3)(box(3, 2))
        assertResult(3)(box(3, 2))
      }
      testIsNotRight(box)
      testImmutable(box)
    }
  }

  describe("A delayed right box") {
    val box = Box.right[Int].delayed(2)
    testIsRight(box)
    testImmutable(box)
  }

  describe("A warrowing built from a delayed widenings") {
    val box = Box.warrowing(intMax.delayed(2), Box.left[Int])
    testIsNotRight(box)
    testMutable(box)
  }
}
