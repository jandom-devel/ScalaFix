/**
 * Copyright 2015, 2017 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains JANDOM
 * is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation,
 * either version 3 of the License, or (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of a MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * JANDOM. If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.scalafix.finite

import java.io.{ByteArrayOutputStream, PrintStream}

import it.unich.scalafix.*
import it.unich.scalafix.graphs.*
import it.unich.scalafix.lattice.Magma
import it.unich.scalafix.lattice.given
import it.unich.scalafix.utils.Relation

import org.scalatest.funspec.AnyFunSpec

class GraphEquationSystemTest extends AnyFunSpec:

  private given Magma[Int] with
    extension (x: Int) def op(y: Int): Int = x max y

  private val unknowns = Set(0, 1, 2, 3)

  private val graph = GraphBody[Int, Int, Char](
    edgeAction = { (rho: Int => Int) =>
      {
        case 'a' => rho(0)
        case 'b' => rho(1) min 10
        case 'c' => rho(2) + 1
        case 'd' => rho(3)
      }
    },
    sources = Relation('a' -> 0, 'b' -> 1, 'c' -> 2, 'd' -> 3),
    target = Map('a' -> 1, 'b' -> 2, 'c' -> 3, 'd' -> 1),
    outgoing = Relation(0 -> 'a', 1 -> 'b', 2 -> 'c', 3 -> 'd'),
    ingoing = Relation(1 -> 'a', 1 -> 'd', 2-> 'b', 3 -> 'c')
  )

  private val simpleEqs = GraphEquationSystem[Int, Int, Char](
    initialGraph = graph,
    unknowns = unknowns,
    inputUnknowns = Set(0)
  )
  private val rho: Assignment[Int, Int] = identity[Int]

  describe("A simple graph equation system") {
    it("correctly computes the body") {
      val body = simpleEqs.body
      assertResult(0)(body(rho)(0))
      assertResult(3)(body(rho)(1))
      assertResult(1)(body(rho)(2))
      assertResult(3)(body(rho)(3))
    }

    it("correctly computes the body with dependencies") {
      val body = simpleEqs.bodyWithDependencies
      assertResult(0 -> Seq())(body(rho)(0))
      assertResult(3 -> Seq(0, 3))(body(rho)(1))
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
      val input: PartialFunction[Int, Int] = { case _ =>
        2
      }
      val eqs = simpleEqs.withBaseAssignment(input)
      val body = eqs.body
      assertResult(2)(body(rho)(0))
      assertResult(3)(body(rho)(1))
      assertResult(2)(body(rho)(2))
      assertResult(3)(body(rho)(3))
    }

    it("correctly adds combos") {
      def test(eqs: FiniteEquationSystem[Int, Int, ?]) =
        val body = eqs.body
        assertResult(0)(body(rho)(0))
        assertResult(7)(body(rho)(1))
        assertResult(4)(body(rho)(2))
        assertResult(9)(body(rho)(3))

      val combo1 = ComboAssignment { (x: Int, y: Int) => x + (2 * y) }
      val combo2 = ComboAssignment({ (x: Int, y: Int) => x + (2 * y) }, false)
      val eqs1 = simpleEqs.withCombos(combo1)
      val eqs2 = simpleEqs.withCombos(combo2)

      test(eqs1)
      test(eqs2)
      for x <- unknowns do
        assert(simpleEqs.infl(x) === eqs1.infl(x))
        // TOOD: check if toSet may be avoided
        assert((simpleEqs.infl(x).toSet + x) === eqs2.infl(x))
    }

    it("correctly adds localized idempotent combos") {
      def test(eqs: FiniteEquationSystem[Int, Int, ?]) =
        val body = eqs.body
        val rho2: Assignment[Int, Int] = { (x: Int) => if x == 0 then 9 else x }
        assertResult(0)(body(rho)(0))
        assertResult(7)(body(rho)(1))
        assertResult(1)(body(rho)(2))
        assertResult(3)(body(rho)(3))
        assertResult(9)(body(rho2)(1))

      val combo1 = ComboAssignment { (x: Int, y: Int) => x + (2 * y) }
      val combo2 = ComboAssignment({ (x: Int, y: Int) => x + (2 * y) }, false)
      val ordering = DFOrdering(simpleEqs)
      val eqs1 = simpleEqs.withLocalizedCombos(combo1, ordering)
      val eqs2 = simpleEqs.withLocalizedCombos(combo2, ordering)
      test(eqs1)
      test(eqs2)
      for x <- unknowns do
        assertResult(simpleEqs.infl(x))(eqs1.infl(x))
        if x != 1 then assertResult(simpleEqs.infl(x))(eqs2.infl(x))
        else
          // TOOD: check if toSet may be avoided
          assertResult(simpleEqs.infl(x).toSet + x)(eqs2.infl(x))
    }

    it("correctly traces equations") {
      val os = new ByteArrayOutputStream()
      val tracingEqs =
        simpleEqs.withTracer(EquationSystemTracer.debug(new PrintStream(os)))
      simpleEqs.body(rho)(0)
      assertResult("")(os.toString)
      os.reset()
      tracingEqs.body(rho)(0)
      assertResult(
        "evaluated: 0 oldvalue: 0\nevaluated: 0 oldvalue: 0 newvalue: 0\n"
      )(os.toString)
      os.reset()
      val combo = ComboAssignment { (x: Int, y: Int) => x + (2 * y) }
      val comboTracingEqs = tracingEqs.withCombos(combo)
      comboTracingEqs.body(rho)(0)
      assertResult(
        "evaluated: 0 oldvalue: 0\nevaluated: 0 oldvalue: 0 newvalue: 0 comboed: 0\n" +
          "evaluated: 0 oldvalue: 0 newvalue: 0\n"
      )(os.toString)
    }
  }
