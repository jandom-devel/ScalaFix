/**
  * Copyright 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.scalafix.*
import it.unich.scalafix.assignments.*
import it.unich.scalafix.finite.SimpleGraphEquationSystem
import it.unich.scalafix.utils.Relation
import it.unich.scalafix.lattice.given

/**
  * This class represents an equation system made of equations `x(i)=x(0) upperbound c(1) uperrbound ... x(n-1)`
  * for i from 0 to n-1.
  *
  * @tparam V type of the values
  * @param n number of unknowns
  */
class CliqueGraphEQS[V](n: Int) extends SimpleGraphEquationSystem[Int, Double, (Int, Int)](
  unknowns = 0 until n,
  inputUnknowns = Set(0),
  edgeAction = { (rho: Assignment[Int, Double]) => (p: (Int, Int)) => rho(p._1) + 1 },
  sources = { (e: (Int, Int)) => Seq(e._1) },
  target = { (e: (Int, Int)) => e._2 },
  outgoing = { (i: Int) => (i + 1 until n) map ((i, _)) },
  ingoing = { (i: Int) => (0 until i) map ((_, i)) }
):

  override val infl: Relation[Int] = Relation({ (i: Int) => (i + 1 until n).toSet })
  override val body: Body[Int, Double] =
    (rho: Assignment[Int, Double]) => (i: Int) => (0 until i) map rho reduce (_ upperBound _)
  override val bodyWithDependencies: BodyWithDependencies[Int, Double] =
    (rho: Assignment[Int, Double]) => (i: Int) => (body(rho)(i), (0 until i).toSet)
