/** Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix

import it.unich.scalafix.assignments.*

import java.io.{ByteArrayOutputStream, PrintStream}

import org.scalatest.funspec.AnyFunSpec

class EquationSystemTest extends AnyFunSpec:

  private val simpleEqs = EquationSystem[Int, Int](
    body = { (rho: Int => Int) =>
      {
        case 0 => rho(0)
        case 1 => (rho(0) max rho(2)) min rho(3)
        case 2 => rho(1) + 1
        case 3 => rho(3)
      }
    },
    initial = identity[Int]
  )
  private val rho: Assignment[Int, Int] = simpleEqs.initial
  private val box: Box[Int] = { (x: Int, y: Int) => x * y }

  describe("An equation system") {
    it("computes r.h.s. according to its body function") {
      assertResult(0)(simpleEqs.body(rho)(0))
      assertResult(2)(simpleEqs.body(rho)(1))
      assertResult(2)(simpleEqs.body(rho)(2))
      assertResult(3)(simpleEqs.body(rho)(3))
    }

    it("correctly infers dependencies") {
      assertResult((0, Seq(0)))(simpleEqs.bodyWithDependencies(rho)(0))
      assertResult((2, Seq(0, 2, 3)))(simpleEqs.bodyWithDependencies(rho)(1))
      assertResult((2, Seq(1)))(simpleEqs.bodyWithDependencies(rho)(2))
      assertResult((3, Seq(3)))(simpleEqs.bodyWithDependencies(rho)(3))
    }

    it("correctly adds boxes") {
      val eqs = simpleEqs.withBoxes(box)
      assertResult(0)(eqs.body(rho)(0))
      assertResult(2)(eqs.body(rho)(1))
      assertResult(4)(eqs.body(rho)(2))
      assertResult(9)(eqs.body(rho)(3))
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
      val boxTracingEqs = tracingEqs.withBoxes(box)
      boxTracingEqs.body(rho)(0)
      assertResult(
        "evaluated: 0 oldvalue: 0\nevaluated: 0 oldvalue: 0 newvalue: 0\nevaluated: 0, " +
          "oldvalue: 0, newvalue: 0, boxed: 0\n"
      )(os.toString)
    }
  }
