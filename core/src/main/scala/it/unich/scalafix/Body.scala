/**
 * Copyright 2015, 2016, 2017, 2022 Gianluca Amato <gianluca.amato@unich.it>
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
 * The body of an equation system, i.e., a map that, given an assignments and an
 * unknown, returns the new value for the unknown.
 */
type Body[U, V] = Assignment[U, V] => Assignment[U, V]

/**
 * A body which also returns the set of dependencies among unknowns. If `body:
 * BodyWithDependencies[U, V]`, `rho: Assignment[U, V]` and `u: U`, then
 * `body(rho)(u)` returns a pair `(v, deps)` where `v` is the new value for `u`
 * and `deps` is a set of unknowns. If `rho'` differs from `rho` only for
 * unknowns which are not in `deps`, then `body(rho)(u)==body(rho')(u)`.
 */
type BodyWithDependencies[U, V] = Assignment[U, V] => U => (V, Iterable[U])

/**
 * Returns a `BodyWithDependencies` by instrumenting the source assignment in
 * order to log access to unknowns.
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
 * Returns a new body with combos added to the evaluation.
 *
 * @param combos
 *   the assignment of combos to unknowns.
 * @param optTracer
 *   an optional tracer to call after evaluation the combo
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
 * Returns a new body, in which the `baseAssignment` assignment is combined with
 * the result of body evaluation trough the use of the `op` combiner.
 *
 * @param baseAssignment
 *   an assignment of values to some of the unknownns.
 * @param op
 *   the operation used to combine the base assignment with the previosu r.h.s.
 *   of the body.
 */
extension [U, V](body: Body[U, V])
  def addBaseAssignment(baseAssignment: PartialFunction[U, V], op: (V, V) => V): Body[U, V] =
    (rho: Assignment[U, V]) =>
      (x: U) =>
        if baseAssignment.isDefinedAt(x)
        then op(baseAssignment(x), body(rho)(x))
        else body(rho)(x)

/**
 * Returns a new body which calls the current tracer before and after
 * evaluation.
 *
 * @param tracer
 *   the tracer to be called by the new body
 */
extension [U, V](body: Body[U, V])
  def addTracer(tracer: EquationSystemTracer[U, V]): Body[U, V] =
    (rho: Assignment[U, V]) =>
      (x: U) =>
        tracer.beforeEvaluation(rho, x)
        val res = body(rho)(x)
        tracer.afterEvaluation(rho, x, res)
        res
