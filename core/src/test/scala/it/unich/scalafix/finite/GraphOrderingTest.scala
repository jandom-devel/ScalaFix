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

package it.unich.scalafix.finite

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

class GraphOrderingTest extends FunSpec with PropertyChecks {

  describe("A trivial graph ordering") {
    it("returns the original sequence") {
      forAll { (s: Set[Int]) =>
        val seq = s.toSeq
        val o = GraphOrdering(seq: _*)
        assertResult(seq)(o.toSeq)
      }
    }
    it("respects the order of the input sequence") {
      forAll { (s: Set[Int]) =>
        whenever(s.size > 0) {
          val seq = s.toSeq
          val o = GraphOrdering(seq: _*)
          val g = Gen.choose(0, s.size - 1)
          forAll(g, g) { (x: Int, y: Int) =>
            assertResult(scala.math.signum(x compare y))(scala.math.signum(o.compare(seq(x), seq(y))))
          }
        }
      }
    }
    it("has only head elements") {
      forAll { (s: Set[Int]) =>
        val seq = s.toSeq
        val o = GraphOrdering(seq: _*)
        for (x <- s) assertResult(true)(o.isHead(x))
      }
    }
  }
}
