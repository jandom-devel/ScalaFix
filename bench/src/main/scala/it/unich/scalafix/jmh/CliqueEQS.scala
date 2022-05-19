/**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it>
 *
 * This file is part of ScalaFix. ScalaFix is free software: you can
 * redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * ScalaFix is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of a MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * ScalaFix. If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.scalafix.jmh

import it.unich.scalafix.*
import it.unich.scalafix.assignments.*
import it.unich.scalafix.finite.*
import it.unich.scalafix.graphs.*
import it.unich.scalafix.utils.*

import scala.collection.mutable

/**
 * This object contains many factory methods which build equation systems made
 * from equations of the form `x(i) = upperBound( x(0), ..., x(n-1) )`.
 */
object CliqueEQS:

  /** Returns a graph based chain equation system with n unknowns. */
  def createGraphEQS[V: Domain](n: Int) =
    val out = (0 until n).map((i: Int) => i -> ((0 until n) map (i -> _))).toMap
    val in = (0 until n).map((i: Int) => i -> ((0 until n) map (_ -> i))).toMap
    GraphEquationSystem[Int, V, (Int, Int)](
      unknowns = 0 until n,
      inputUnknowns = Set(0),
      initialGraph = GraphBody(
        edgeAction = (rho: Assignment[Int, V]) => (p: (Int, Int)) => rho(p._1),
        sources = Relation((e: (Int, Int)) => Seq(e._1)),
        target = (e: (Int, Int)) => e._2,
        outgoing = Relation(out),
        ingoing = Relation(in),
        combiner = summon[Domain[V]].upperBound
      )
    )

  /** Returns a finite equation system with n unknowns. */
  def createFiniteEQS[V: Domain](n: Int) = FiniteEquationSystem[Int, V](
    unknowns = 0 until n,
    inputUnknowns = Set(0),
    initialInfl = Relation((i: Int) => 0 until n),
    initialBody = (rho: Assignment[Int, V]) =>
      (i: Int) => (0 until n) map rho reduce summon[Domain[V]].upperBound
  )
