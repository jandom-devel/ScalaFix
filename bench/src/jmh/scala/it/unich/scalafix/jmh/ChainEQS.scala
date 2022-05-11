/**
 * Copyright 2016, 2017, 2022 Gianluca Amato <gianluca.amato@unich.it>
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

/**
 * This class represents an equation system made of equations `x(i+1)=x(i)` for
 * i from 0 to n-1. This is a very optimized version of the equation system
 * where all attributes are specified explicitly.
 *
 * @tparam V
 *   type of the values
 * @param n
 *   number of unknowns
 */
class ChainGraphEQS[V: Domain](n: Int)
    extends SimpleGraphEquationSystem[Int, V, Int](
      unknowns = 0 until n,
      inputUnknowns = Set(0),
      initialGraph = GraphBody(
        edgeAction = (rho: Assignment[Int, V]) => (i: Int) => rho(i),
        sources = Relation((i: Int) => Set(i)),
        target = (i: Int) => i + 1,
        outgoing = Relation((i: Int) => if i == n - 1 then Set.empty else Set(i)),
        ingoing = Relation((i: Int) => if i == 0 then Set.empty else Set(i - 1)),
        combiner = summon[Domain[V]].upperBound
      )
    ):
  override val infl: Relation[Int, Int] = Relation((i: Int) => Set(i + 1))
  override val body: Body[Int, V] = { (rho: Assignment[Int, V]) => (i: Int) =>
    if i > 0 then rho(i - 1) else rho(0)
  }
  override val bodyWithDependencies: BodyWithDependencies[Int, V] =
    (rho: Assignment[Int, V]) =>
      (i: Int) => if i > 0 then (rho(i - 1), Seq(i - 1)) else (rho(0), Seq(0))

/**
 * This class represents an equation system made of equations `x(i+1)=x(i)` for
 * i from 0 to n-1. This is an un-optimized version of the equation system using
 * SimpleGraphEquationSystem.
 *
 * @tparam V
 *   type of the values
 * @param n
 *   number of unknowns
 */
class ChainSimpleGraphEQS[V: Domain](n: Int)
    extends SimpleGraphEquationSystem[Int, V, Int](
      unknowns = 0 until n,
      inputUnknowns = Set(0),
      initialGraph = GraphBody(
        edgeAction = (rho: Assignment[Int, V]) => (i: Int) => rho(i),
        sources = Relation((i: Int) => Set(i)),
        target = (i: Int) => i + 1,
        outgoing = Relation((i: Int) => Set(i)),
        ingoing = Relation((i: Int) => if i == 0 then Set.empty else Set(i - 1)),
        combiner = summon[Domain[V]].upperBound
      )
    )

/**
 * This class represents an equation system made of equations `x(i+1)=x(i)` for
 * i from 0 to n-1. This is an un-optimized version of the equation system using
 * SimpleFiniteGraphEquationSystem.
 *
 * @tparam V
 *   type of the values
 * @param n
 *   number of unknowns
 */
class ChainSimpleFiniteEQS[V](n: Int)
    extends SimpleFiniteEquationSystem[Int, V](
      initialBody = (rho: Assignment[Int, V]) => (i: Int) => if i > 0 then rho(i - 1) else rho(0),
      inputUnknowns = Set(0),
      unknowns = 0 to n,
      initialInfl = Relation( (i: Int) => Set(i + 1) )
    )

/**
 * This class represents an infinite equation system made of equations
 * `x(i+1)=x(i)`. This is an optimized version where the attribute
 * bodyWithDependencies is provided explicitly.
 *
 * @tparam V
 *   type of the values
 */
class ChainInfiniteEQS[V]
    extends SimpleEquationSystem[Int, V](
      initialBody = (rho: Assignment[Int, V]) => (i: Int) => if i > 0 then rho(i - 1) else rho(0)
    ):

  override val bodyWithDependencies: BodyWithDependencies[Int, V] =
    (rho: Assignment[Int, V]) => (i: Int) => (rho(i - 1), if i > 0 then Seq(i - 1) else Seq.empty)

/**
 * This class represents an infinite equation system made of equations
 * `x(i+1)=x(i)`. This is an un-optimized version where the attribute
 * bodyWithDependencies is computed automatically.
 *
 * @tparam V
 *   type of the values
 */
class ChainInfinite2EQS[V]
    extends SimpleEquationSystem[Int, V](
      initialBody = (rho: Assignment[Int, V]) => (i: Int) => if i > 0 then rho(i - 1) else rho(0)
    )
