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

import it.unich.scalafix.FixpointSolver
import it.unich.scalafix.FixpointSolverTracer.PerformanceFixpointSolverTracer
import it.unich.scalafix.assignments.InputAssignment
import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks

class FiniteFixpointSolverTest extends FunSpec with PropertyChecks {

  import FixpointSolver._

  private val simpleEqs = GraphEquationSystem[Int, Double, Char](
    edgeAction = { rho: (Int => Double) => {
      case 'a' => rho(0)
      case 'b' => rho(1) min 10
      case 'c' => rho(2) + 1
      case 'd' => rho(3)
    }
    },
    source = Map(('a', Seq(0)), ('b', Seq(1)), ('c', Seq(2)), ('d', Seq(3))),
    target = Map(('a', 1), ('b', 2), ('c', 3), ('d', 1)),
    outgoing = Map((0, Seq('a')), (1, Seq('b')), (2, Seq('c')), (3, Seq('d'))),
    ingoing = Map((0, Seq()), (1, Seq('a', 'd')), (2, Seq('b')), (3, Seq('c'))),
    unknowns = Set(0, 1, 2, 3),
    inputUnknowns = Set(0),
    initial = InputAssignment.conditional(0, 0.0, Double.NegativeInfinity)
  )
  private val solution = Map[Int, Double](0 -> 0, 1 -> 11, 2 -> 10, 3 -> 11)
  private val emptysol = (0 to 3).map { u: Int => u -> Double.NegativeInfinity }.toMap
  private val onlyWideningSol = Map[Int, Double](0 -> 0, 1 -> Double.PositiveInfinity, 2 -> 10, 3 -> 11)
  private val doublewidening = { (x: Double, y: Double) => if (x.isNegInfinity) y else if (x >= y) x else Double.PositiveInfinity }
  private val doublenarrowing = { (x: Double, y: Double) => if (x.isPosInfinity) y else x }
  private val CC77params = FiniteFixpointSolver.CC77[Int, Double](Solver.WorkListSolver, doublewidening, doublenarrowing)

  def assertSolution(m: Map[Int, Double])(b: Int => Double): Unit = {
    for (u <- simpleEqs.unknowns) assertResult(m(u), s"at unknown $u")(b(u))
  }

  class ValidationListener[U, V] extends PerformanceFixpointSolverTracer[U, V] {
    var initialized = false
    var phase = 0

    override def evaluated(rho: U => V, u: U, newval: V): Unit = {
      assert(initialized)
      assert(phase != 0)
      super.evaluated(rho, u, newval)
    }

    override def initialized(rho: U => V): Unit = {
      assert(!initialized)
      assert(phase != 0)
      initialized = true
    }

    override def completed(rho: U => V): Unit = {
      initialized = false
      assert(phase != 0)
      ()
    }

    override def ascendingBegins(rho: U => V): Unit = {
      assert(!initialized)
      assert(phase == 0)
      phase = 1
    }

    override def descendingBegins(rho: U => V): Unit = {
      assert(!initialized)
      assert(phase == 1)
      phase = -1
    }
  }

  describe("The finite driver") {
    it("may be called with CC77 parameters") {
      val params = CC77params
      assertSolution(solution)(FiniteFixpointSolver(simpleEqs, params))
    }

    it("may use explicit initial assignment") {
      val params = CC77params.copy[Int, Double](start = Some(Double.NegativeInfinity))
      assertSolution(emptysol)(FiniteFixpointSolver(simpleEqs, params))
    }

    it("may use a priority worklist solver") {
      val params = CC77params.copy[Int, Double](solver = Solver.PriorityWorkListSolver)
      assertSolution(solution)(FiniteFixpointSolver(simpleEqs, params))
    }

    it("may use a hierarchical ordering solver") {
      val params = CC77params.copy[Int, Double](solver = Solver.HierarchicalOrderingSolver)
      assertSolution(solution)(FiniteFixpointSolver(simpleEqs, params))
    }

    it("may avoid the descending chain") {
      val params = CC77params.copy(boxstrategy = BoxStrategy.OnlyWidening)
      assertSolution(onlyWideningSol)(FiniteFixpointSolver(simpleEqs, params))
    }

    it("calls the fixpoit solver listener") {
      val t = new ValidationListener[Int, Double]
      val params = CC77params.copy(tracer = t)
      assertSolution(solution)(FiniteFixpointSolver(simpleEqs, params))
      assert(t.evaluations > 0)
    }
  }
}
