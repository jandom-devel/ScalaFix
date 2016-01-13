/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
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

package it.unich.scalafix.structured

import it.unich.scalafix.Box
import org.scalatest.FunSpec
import it.unich.scalafix.finite.DFOrdering
import it.unich.scalafix.finite.FiniteEquationSystem
import it.unich.scalafix.utils.Relation
import it.unich.scalafix.lattice.Magma
import scala.runtime.RichInt

class FlowEquationSystemTest extends FunSpec {

  implicit object MagmaInt extends Magma[Int] {
    def op(x: Int, y: Int) = x max y
  }

  val edges = Set('a', 'b', 'c', 'd')
  val unknowns = Set(0, 1, 2, 3)
  val simpleEqs = FlowEquationSystem[Int, Int, Char](
    edgeAction = {
      { (rho: Int => Int) =>
        {
          (e: Char) =>
            e match {
              case 'a' => rho(0)
              case 'b' => rho(1) min 10
              case 'c' => rho(2) + 1
              case 'd' => rho(3)
            }
        }
      }
    },
    source = Map(('a', 0), ('b', 1), ('c', 2), ('d', 3)),
    target = Map(('a', 1), ('b', 2), ('c', 3), ('d', 1)),
    outgoing = Map((0,Seq('a')), (1,Seq('b')), (2,Seq('c')), (3, Seq('d'))),
    ingoing =  Map((0,Seq()), (1,Seq('a','d')), (2,Seq('b')), (3, Seq('c'))),
    unknowns = unknowns,
    inputUnknowns = Set(0),
    initial = { _ => 0 })
  val rho = { x: Int => x }

  describe("A simple flow equation system") {
    it("correctly computes the body") {
      val body = simpleEqs.body
      assertResult(0)(body(rho)(0))
      assertResult(3)(body(rho)(1))
      assertResult(1)(body(rho)(2))
      assertResult(3)(body(rho)(3))
    }

    it("correctly computes the body with dependencies") {
      val body = simpleEqs.bodyWithDependencies _
      assertResult(0 -> Seq())(body(rho)(0))
      assertResult(3 -> Seq(0,3))(body(rho)(1))
      assertResult(1 -> Seq(1))(body(rho)(2))
      assertResult(3 -> Seq(2))(body(rho)(3))
    }

    it("correctly computes the influence") {
      val infl = simpleEqs.infl
      assertResult(Set(1))(infl(0))
      assertResult(Set(2))(infl(1))
      assertResult(Set(3))(infl(2))
      assertResult(Set(1))(infl(3))
    }

    it("correctly adds input assignments") {
      val input: PartialFunction[Int, Int] = { case _ => 2 }
      val eqs = simpleEqs.withBaseAssignment(input)
      val body = eqs.body
      assertResult(2)(body(rho)(0))
      assertResult(3)(body(rho)(1))
      assertResult(2)(body(rho)(2))
      assertResult(3)(body(rho)(3))
    }

    it("correctly adds boxes") {
      def test(eqs: FiniteEquationSystem[Int, Int]) = {
        val body = eqs.body
        assertResult(0)(body(rho)(0))
        assertResult(7)(body(rho)(1))
        assertResult(4)(body(rho)(2))
        assertResult(9)(body(rho)(3))
      }

      val box1: Box[Int] = { (x: Int, y: Int) => x + (2 * y) }
      val box2: Box[Int] = Box ( { (x: Int, y: Int) => x + (2 * y) }, false )
      val eqs1 = simpleEqs.withBoxes(box1)
      val eqs2 = simpleEqs.withBoxes(box2)

      test(eqs1)
      test(eqs2)
      for (x <- unknowns) {
        assert(simpleEqs.infl(x) === eqs1.infl(x))
        assert((simpleEqs.infl(x) + x) === eqs2.infl(x))
      }
    }

    it("correctly adds localized idempotent boxes") {
      def test(eqs: FiniteEquationSystem[Int, Int]) = {
        val body = eqs.body
        val rho2 = { x: Int => if (x == 0) 9 else x }
        assertResult(0)(body(rho)(0))
        assertResult(7)(body(rho)(1))
        assertResult(1)(body(rho)(2))
        assertResult(3)(body(rho)(3))
        assertResult(9)(body(rho2)(1))
      }

      val box1: Box[Int] = { (x: Int, y: Int) => x + (2 * y) }
      val box2: Box[Int] = Box ( { (x: Int, y: Int) => x + (2 * y) }, false )
      val ordering = DFOrdering(simpleEqs)
      val eqs1 = simpleEqs.withLocalizedBoxes(box1, ordering)
      val eqs2 = simpleEqs.withLocalizedBoxes(box2, ordering)

      test(eqs1)
      test(eqs2)
      for (x <- unknowns) {
        assert(simpleEqs.infl(x) === eqs1.infl(x))
        if (x != 1)
          assert(simpleEqs.infl(x) === eqs2.infl(x))
        else
          assert((simpleEqs.infl(x) + x) === eqs2.infl(x))
      }
    }
  }
}

