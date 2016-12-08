/**
  * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.scalafix.jmh

import it.unich.scalafix.{Body, _}
import it.unich.scalafix.finite.{FiniteEquationSystem, GraphEquationSystem}
import it.unich.scalafix.utils.Relation

/**
  * This class represents an equation system made of equations `x(i)=x(0) \/ c(1) \/ ... x(n-1)` for i from 0
  * to n-1.
  * @tparam V type of the values
  * @param n number of unknowns
  * @param v the initial value for all unknowns
  */
class CliqueGraphEQS[V](n: Int, v: Double) extends GraphEquationSystem[Int,Double,(Int, Int)]
  with GraphEquationSystem.WithLocalizedBoxes[Int,Double,(Int,Int)] with FiniteEquationSystem.WithBoxes[Int,Double]
  with FiniteEquationSystem.WithBaseAssignment[Int,Double] {
  val unknowns = 0 until n
  val inputUnknowns = Set(0)
  val edgeAction = { (rho: Assignment[Int,Double]) => p: (Int,Int) => rho(p._1)  + 1 }
  val sources = { e: (Int,Int) => Seq(e._1) }
  val target = { e: (Int,Int) => e._2 }
  val outgoing = { (i: Int) => (i+1 until n) map { (i,_) } }
  val ingoing =  { (i: Int) => (0 until i) map { (_,i) } }
  val initial = { (x: Int) => v }
  val infl = Relation( { (i: Int) => (i+1 until n).toSet } )
  val body = Body( { rho: Assignment[Int,Double] => { i: Int => (0 until i) map { rho(_) } reduce dom.upperBound }  } )
  val bodyWithDependencies = { rho: Assignment[Int,Double] => { (i: Int) => (body(rho)(i), (0 until i).toSet) } }
}

