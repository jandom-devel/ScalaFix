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

class BoxAssignmentTest extends FunSpec with PropertyChecks {

  val maxbox: Box[Int] = { (x: Int, y: Int) => x max y }

  def everywhere[V](boxes: BoxAssignment[Int, V]) =
    it("is defined everywhere") {
      forAll { (u: Int) =>
        assertResult(true)(boxes.isDefinedAt(u))
      }
    }

  def isEmpty(boxes: BoxAssignment[Nothing, _]) =
    it("is empty") {
      assertResult(true)(boxes.isEmpty)
    }

  def isNotEmpty(boxes: BoxAssignment[Nothing, _]) =
    it("is not empty") {
      assertResult(false)(boxes.isEmpty)
    }

  def isIdempotent(boxes: BoxAssignment[Nothing, _]) = {
    it("is idempotent") {
      assertResult(true)(boxes.boxesAreIdempotent)
    }
  }

  def isNotIdempotent(boxes: BoxAssignment[Nothing, _]) = {
    it("is not idempotent") {
      assertResult(false)(boxes.boxesAreIdempotent)
    }
  }

  def copyIdempotent(boxes: BoxAssignment[Nothing, _]) = {
    it("has a copy method which returns itself") {
      assertResult(boxes)(boxes.copy)
    }
  }

  def copyNotIdempotent(boxes: BoxAssignment[Nothing, _]) = {
    it("has a copy method which returns a new object") {
      assert(boxes != boxes.copy)
    }
  }

  describe("The empty box assignment") {
    val boxes = BoxAssignment.empty[Int]
    it("it returns a right box for each unknown") {
      forAll { (u: Int) =>
        assertResult(true)(boxes(u).isRight)
      }
    }
    it("is defined nowhere") {
      forAll { (u: Int) =>
        assertResult(true)(!boxes.isDefinedAt(u))
      }
    }
    isEmpty(boxes)
    isIdempotent(boxes)
    copyIdempotent(boxes)
  }

  describe("The constant box assignment for the min box") {
    val boxes: BoxAssignment[Int, Int] = { (x: Int, y: Int) => x min y }
    it("returns the same box for each unknown") {
      forAll { (u: Int, v: Int) =>
        assertResult(true)(boxes(u) eq boxes(v))
      }
    }
    it("returns the correct box") {
      forAll { (u: Int, x: Int, y: Int) =>
        assertResult(x min y)(boxes(u)(x, y))
      }
    }
    isNotEmpty(boxes)
    everywhere(boxes)
    isIdempotent(boxes)
    copyIdempotent(boxes)
  }

  describe("The box assignment built from an immutable idempotent box") {
    val boxes: BoxAssignment[Int, Int] = maxbox
    it("returns the same box for each unknown") {
      forAll { (u: Int) =>
        assertResult(true)(maxbox eq boxes(u))
      }
    }
    it("returns the same box each time it is applied to the same unknown") {
      forAll { (u: Int) =>
        assertResult(true)(boxes(u) eq boxes(u))
      }
    }
    isNotEmpty(boxes)
    everywhere(boxes)
    isIdempotent(boxes)
    copyIdempotent(boxes)
  }


  describe("The box assignment built from a mutable box") {
    val maxboxDelayed: Box[Int] = maxbox.delayed(2)
    val boxes: BoxAssignment[Int, Int] = maxboxDelayed
    it("returns a different box for each unknown") {
      forAll { (u: Int) =>
        assertResult(false)(maxboxDelayed eq boxes(u))
        forAll { (v: Int) =>
          whenever(u != v) {
            assertResult(false)(boxes(v) eq boxes(u))
          }
        }
      }
    }
    it("returns the same box each time it is applied to the same unknown") {
      forAll { (u: Int) =>
        assertResult(true)(boxes(u) eq boxes(u))
      }
    }
    isNotEmpty(boxes)
    everywhere(boxes)
    isNotIdempotent(boxes)
    copyNotIdempotent(boxes)
  }
}
