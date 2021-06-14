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
import it.unich.scalafix.finite.*
import it.unich.scalafix.lattice.Domain
import it.unich.scalafix.utils.Relation

/**
  * This class represents an equation system made of equations `x(i+1)=x(i)` for i from 0 to n-1. This is a very
  * optimized version of the equation system where all attributes are specified explicitly.
  *
  * @tparam V type of the values
  * @param n number of unknowns
  * @param v the initial value for all unknowns
  */
class ChainGraphEQS[V: Domain](n: Int, v: V) extends SimpleGraphEquationSystem[Int, V, Int](
  unknowns = 0 until n,
  inputUnknowns = Set(0),
  edgeAction = { (rho: Assignment[Int, V]) => (i: Int) => rho(i) },
  sources = { (i: Int) => Seq(i) },
  target = { (i: Int) => i + 1 },
  outgoing = { (i: Int) => if (i == n - 1) Seq.empty else Seq(i) },
  ingoing = { (i: Int) => if (i == 0) Seq.empty else Seq(i - 1) },
  initial = v
) {
  override val infl: Relation[Int] = Relation({ (i: Int) => Set(i + 1) })
  override val body: Body[Int, V] = { (rho: Assignment[Int, V]) => (i: Int) => if (i > 0) rho(i - 1) else rho(0) }
  override val bodyWithDependencies: BodyWithDependencies[Int, V] = {
    (rho: Assignment[Int, V]) => (i: Int) => if (i > 0) (rho(i - 1), Seq(i - 1)) else (rho(0), Seq(0))
  }
}

/**
  * This class represents an equation system made of equations `x(i+1)=x(i)` for i from 0 to n-1. This is an
  * un-optimized version of the equation system using SimpleGraphEquationSystem.
  *
  * @tparam V type of the values
  * @param n number of unknowns
  * @param v the initial value for all unknowns
  */
class ChainSimpleGraphEQS[V: Domain](n: Int, v: V) extends SimpleGraphEquationSystem[Int, V, Int](
  unknowns = 0 until n,
  inputUnknowns = Set(0),
  edgeAction = { (rho: Assignment[Int, V]) => (i: Int) => rho(i) },
  sources = { (i: Int) => Seq(i) },
  target = { (i: Int) => i + 1 },
  outgoing = { (i: Int) => Seq(i) },
  ingoing = { (i: Int) => if (i == 0) Seq.empty else Seq(i - 1) },
  initial = v,
)

/**
  * This class represents an equation system made of equations `x(i+1)=x(i)` for i from 0 to n-1. This is an
  * un-optimized version of the equation system using SimpleFiniteGraphEquationSystem.
  *
  * @tparam V type of the values
  * @param n number of unknowns
  * @param v the initial value for all unknowns
  */
class ChainSimpleFiniteEQS[V](n: Int, v: V) extends SimpleFiniteEquationSystem[Int, V](
  body = { (rho: Assignment[Int, V]) => (i: Int) => if (i > 0) rho(i - 1) else rho(0) },
  initial = v,
  inputUnknowns = Set(0),
  unknowns = 0 to n,
  infl = Relation({ (i: Int) => Set(i + 1) })
)

/**
  * This class represents an infinite equation system made of equations `x(i+1)=x(i)`. This is an optimized version
  * where the attribute bodyWithDependencies is provided explicitly.
  *
  * @tparam V type of the values
  * @param v the initial value for all unknowns
  */
class ChainInfiniteEQS[V](v: V) extends SimpleEquationSystem[Int, V](
  body = { (rho: Assignment[Int, V]) => (i: Int) => if (i > 0) rho(i - 1) else rho(0) }: Body[Int, V],
  initial = v,
  inputUnknowns = Set(0)
) {
  override val bodyWithDependencies: BodyWithDependencies[Int, V] = {
    (rho: Assignment[Int, V]) => (i: Int) => (rho(i - 1), if (i > 0) Seq(i - 1) else Seq.empty)
  }
}

/**
  * This class represents an infinite equation system made of equations `x(i+1)=x(i)`. This is an un-optimized version
  * where the attribute bodyWithDependencies is computed automatically.
  *
  * @tparam V type of the values
  * @param v the initial value for all unknowns
  */
class ChainInfinite2EQS[V](v: V) extends SimpleEquationSystem[Int, V](
  body = { (rho: Assignment[Int, V]) => (i: Int)   => if (i > 0) rho(i - 1) else rho(0) }: Body[Int, V],
  initial = v,
  inputUnknowns = Set(0)
)
