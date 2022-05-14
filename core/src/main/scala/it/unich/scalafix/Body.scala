 /**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it> and
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

package it.unich.scalafix

import scala.collection.mutable

/**
 * The body of an equation system, i.e., a map that, given an assignment and an
 * unknown, returns the new value for the unknown.
 *
 * @tparam U
 *   the type for the unknowns.
 * @tparam V
 *   the type for the values assumed by the unknowns.
 */
type Body[U, V] = Assignment[U, V] => Assignment[U, V]

/**
 * A body which also returns the set of dependencies among unknowns. If `bd` is
 * a body with dependencies, `rho` an assignment and `u` an unknown, then
 * `bd(rho)(u)` returns a pair `(v, deps)` where `v` is the new value for `u`
 * and `deps` is a set of unknowns. If `rho'` differs from `rho` only for
 * unknowns which are not in `deps`, then `bd(rho)(u)==bd(rho')(u)`.
 *
 * @tparam U
 *   the type for the unknowns.
 * @tparam V
 *   the type for the values assumed by the unknowns.
 */
type BodyWithDependencies[U, V] = Assignment[U, V] => U => (V, Iterable[U])

/**
 * Returns a body with dependencies by instrumenting the input assignment in
 * order to log access to unknowns.
 *
 * @tparam U
 *   the type for the unknowns.
 * @tparam V
 *   the type for the values assumed by the unknowns.
 */
extension [U, V](body: Body[U, V])
  def withDependencies: BodyWithDependencies[U, V] =
    (rho: Assignment[U, V]) =>
      (x: U) =>
        val queried = mutable.Buffer.empty[U]
        val trackrho = (y: U) =>
          queried.append(y)
          rho(y)
        val newval = body(trackrho)(x)
        (newval, queried)

/**
 * Returns a new body obtained by adding combos to this body. If `x` in an
 * unknown, `rho` an assignment, and `combos(x) = Some(op)`, then the new body
 * for the assignment `rho` and unknown `x` returns `rho(x) op body(rho)(x)`.
 *
 * @tparam U
 *   the type for the unknowns.
 * @tparam V
 *   the type for the values assumed by the unknowns.
 * @param combos
 *   the assignment of combos to unknowns.
 * @param optTracer
 *   an optional tracer to call after evaluation of the combos.
 */
extension [U, V](body: Body[U, V])
  def addCombos(
      combos: ComboAssignment[U, V],
      optTracer: Option[EquationSystemTracer[U, V]] = None
  ): Body[U, V] =
    if combos.isEmpty
    then body
    else
      val realCombos = combos.copy
      (rho: Assignment[U, V]) =>
        (x: U) =>
          val res = body(rho)(x)
          if realCombos.isDefinedAt(x) then
            val comboedRes = realCombos(x)(rho(x), res)
            optTracer foreach (_.comboEvaluation(rho, x, res, comboedRes))
            comboedRes
          else res

/**
 * Returns a new body in which a base ssignment is combined with this body
 * trough the use of the `op` operator. If `x` in an unknown, `rho` an
 * assignment, and `baseAssignment(x) = y`, then the new body for the for the
 * assignment `rho` and unknown `x` returns `body(rho)(x) op y`.
 *
 * @tparam U
 *   the type for the unknowns.
 * @tparam V
 *   the type for the values assumed by the unknowns.
 * @param baseAssignment
 *   a partial assignment of values to unknownns.
 * @param op
 *   the operation used to combine the base assignment with the rhs of the body.
 */
extension [U, V](body: Body[U, V])
  def addBaseAssignment(baseAssignment: PartialFunction[U, V], op: (V, V) => V): Body[U, V] =
    (rho: Assignment[U, V]) =>
      (x: U) =>
        if baseAssignment.isDefinedAt(x)
        then op(baseAssignment(x), body(rho)(x))
        else body(rho)(x)

/**
 * Returns a new body which calls the specified tracer before and after
 * evaluation of the rhs of the body.
 *
 * @tparam U
 *   the type for the unknowns.
 * @tparam V
 *   the type for the values assumed by the unknowns.
 * @param tracer
 *   the tracer to be called by the new body.
 */
extension [U, V](body: Body[U, V])
  def addTracer(tracer: EquationSystemTracer[U, V]): Body[U, V] =
    (rho: Assignment[U, V]) =>
      (x: U) =>
        tracer.beforeEvaluation(rho, x)
        val res = body(rho)(x)
        tracer.afterEvaluation(rho, x, res)
        res
