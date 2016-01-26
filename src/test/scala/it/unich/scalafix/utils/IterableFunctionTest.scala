/**
  * Copyright 2015, 2016 Gianluca Amato <gamato@unich.it>
  *
  * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
  * JANDOM is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * JANDOM is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.utils

import org.scalatest.prop.PropertyChecks
import org.scalatest.FunSpec

class IterableFunctionTest extends FunSpec with PropertyChecks {
  describe("An empty iterable function") {
    val f = IterableFunction.empty[Int, Int]
    it("is empty") {
      assert(f.isEmpty)
    }
    it("has empty keys") {
      assert(f.keys.isEmpty)
    }
    it("returns an exception for every input") {
      forAll { (x: Int) =>
        intercept[NoSuchElementException](f(x))
        ()  // why we need this unit literale ??
      }
    }
  }

  describe("An interable function derived from a map") {
    it("has the same functional behaviour of the map") {
      forAll { (m: Map[Int, Int]) =>
        val f: IterableFunction[Int, Int] = m
        for ((x, y) <- m.toSeq)
          assertResult(y)(f(x))
        assertResult(m.keys)(f.keys)
        assertResult(m.mkString)(f.mkString)
      }
    }
    it("returns an exception for input not in the map") {
      forAll { (m: Map[Int, Int], i: Int) =>
        val f: IterableFunction[Int, Int] = m
        whenever(!m.isDefinedAt(i)) {
          intercept[NoSuchElementException](f(i))
        }
      }
    }
  }
}
