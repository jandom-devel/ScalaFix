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
import it.unich.scalafix.graphs.GraphBodyBuilder.*
import it.unich.scalafix.utils.*

import java.lang.Math.floorMod

import scala.collection.mutable

/**
 * This class represents a graph based equation system whose unknowns are
 * integers from `0` to `n-1` and each equation has the form `rho(i) =
 * op(rho((i-m) mod n)) ⨆ ... ⨆ op(rho((i-1) mod n))` where `x mod n` is the
 * POSITIVE remainder of the integer division `x/n`, `⨆` is the upper bound of
 * the domain `V` and `op` is an operator which defaults to identity.
 *
 * When `m = n-1`, the graph underlying the equation system is a complete
 * directed graph.
 *
 * @param n
 *   number of unknowns
 * @param m
 *   out-degree of each node
 * @param op
 *   trasformation operator, default is identity
 */
class GraphCliqueEQS[V: Domain](n: Int, m: Int, op: V => V = identity[V])
    extends BaseGraphEquationSystem[Int, V, (Int, Int), GraphCliqueEQS[V]]:

  private val out =
    for i <- 0 until n
    yield for j <- 1 to m
    yield i -> ((i + j) % n)

  private val in =
    for i <- 0 until n
    yield for j <- 1 to m
    yield floorMod(i - j, n) -> i

  override val inputUnknowns = Seq(0)

  override val initialGraph = GraphBody(
    edgeAction = (rho: Assignment[Int, V]) => p => op(rho(p._1)),
    sources = Relation(p => Seq(p._1)),
    target = p => p._2,
    outgoing = Relation(out),
    ingoing = Relation(in),
    unknowns = 0 until n,
    combiner = summon[Domain[V]].upperBound
  )

/**
 * This is similar to [[GraphCliqueEQS]], but the result is a finite equation
 * system instead of a graph based equation system.
 */
class FiniteCliqueEQS[V: Domain](n: Int, m: Int, op: V => V = identity[V])
    extends BaseFiniteEquationSystem[Int, V, FiniteCliqueEQS[V]]:

  override val unknowns = 0 until n
  override val inputUnknowns = Seq(0)
  override val initialInfl = Relation((i: Int) => (1 to m) map (j => (i + j) % n))
  override val initialBody = (rho: Assignment[Int, V]) =>
    (i: Int) => (1 to m) map (j => op(rho(floorMod(i - j, n)))) reduce summon[Domain[V]].upperBound

/**
 * This is similar to [[GraphCliqueEQS]] but it works using a graph body builder
 * instead of a simple graph body.
 */
class GraphBuilderCliqueEQS[V: Domain](n: Int, m: Int, op: V => V = identity[V])
    extends BaseGraphBuilderEquationSystem[Int, (Int, Int), V]:

  override val builder = GraphBodyBuilder[Int, (Int, Int), V]()
  override val combiner = summon[Domain[V]].upperBound

  private val nodes = new Array[builder.U](n)
  for u <- 0 until n do nodes(u) = builder.addNode(u)
  for
    u <- 0 until n
    e <- 1 to m
    v = (u + e) % n
  do
    builder.addEdge(
      (u, v),
      Seq(nodes(u)),
      nodes(v),
      (rho: Assignment[builder.U, V]) => op(rho(nodes(u)))
    )

  override val inputUnknowns = Seq(nodes(0))

/**
 * This object contains many factory methods which build equation systems based
 * on [[GraphCliqueEQS]], [[FiniteCliqueEQS]] and [[GraphBuilderCliqueEQS]].
 */
object CliqueEQS:

  /**
   * Builds a full [[GraphCliqueEQS]] with `n` unknowns, nodes of out-degree
   * `n-1` and identity transformation operator.
   */
  def createGraphEQS[V: Domain](n: Int): GraphCliqueEQS[V] = GraphCliqueEQS(n, n - 1)

  /**
   * Builds a [[GraphCliqueEQS]] with `n` unknowns, nodes of out-degree `m` and
   * `op` as the transformation operator.
   */
  def createGraphEQS[V: Domain](n: Int, m: Int, op: V => V = identity[V]): GraphCliqueEQS[V] =
    GraphCliqueEQS(n, m, op)

  /**
   * Builds a [[FiniteCliqueEQS]] with `n` unknowns, each depending on all the
   * other unknowns, and identity transformation operator.
   */
  def createFiniteEQS[V: Domain](n: Int): FiniteCliqueEQS[V] = FiniteCliqueEQS(n, n - 1)

  /**
   * Builds a [[FiniteCliqueEQS]] with `n` unknowns, each depending on the
   * previous `m` unknowns, and `op` as the transformation operator.
   */
  def createFiniteEQS[V: Domain](n: Int, m: Int, op: V => V = identity[V]): FiniteCliqueEQS[V] =
    FiniteCliqueEQS(n, m, op)

  /**
   * Builds a [[GraphBuilderCliqueEQS]] with `n` unknowns, each depending on all
   * the other unknowns, and identity transformation operator.
   */
  def createGraphEQSBuilder[V: Domain](n: Int): GraphBuilderCliqueEQS[V] =
    GraphBuilderCliqueEQS(n, n - 1, identity[V])

  /**
   * Builds a [[GraphBuilderCliqueEQS]] with `n` unknowns, nodes of out-degree
   * `m` and `op` as the transformation operator.
   */
  def createGraphBuilderEQS[V: Domain](n: Int, m: Int, op: V => V): GraphBuilderCliqueEQS[V] =
    GraphBuilderCliqueEQS(n, m, op)
