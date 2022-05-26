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

package it.unich.scalafix.graphs

import it.unich.scalafix.*

/**
 * The base abstract implementation for a graph-based equation system built from
 * a graph body builder. Equation systems built using this class authmatically
 * get assignments implemented directly as fields of nodes, which should be
 * faster than hashmap based assignments.
 *
 * @tparam LN
 *   type of labels for nodes.
 * @tparam LE
 *   type of labels for edges.
 * @tparam V
 *   the type for the values assumed by the unknowns.
 */
abstract class BaseGraphBuilderEquationSystem[LN, LE, V]
    extends BaseGraphEquationSystem[
      GraphBodyBuilder.Node[LN, LE, V],
      V,
      GraphBodyBuilder.Edge[LN, LE, V],
      BaseGraphBuilderEquationSystem[LN, LE, V]
    ]:

  /** The operation used from combining the contributions of different edges. */
  val combiner: (V, V) => V

  /** The graph body builder. */
  val builder: GraphBodyBuilder[LN, LE, V]

  override protected def initialGraph: GraphBody[builder.U, V, builder.E] =
    builder.toGraphBody(combiner)
  override def getMutableAssignment(rho: Assignment[builder.U, V]) =
    builder.GraphMutableAssignment(rho)

/**
 * Default implementation of a graph-based equation system given by a graph body
 * builder.
 *
 * @tparam LN
 *   type of labels for nodes
 * @tparam LE
 *   type of labels for edges
 * @tparam V
 *   the type for the values assumed by the unknowns
 * @param builder
 *   the graph body builder
 * @param combiner
 *   the operation used from combining the contributions of different edges.
 * @param inputUnknowns
 *   the unknowns which may be considered the input to this equation system.
 */
class SimpleGraphBuilderEquationSystem[LN, LE, V](
    override val builder: GraphBodyBuilder[LN, LE, V],
    override val combiner: (V, V) => V,
    override val inputUnknowns: Iterable[builder.U]
) extends BaseGraphBuilderEquationSystem[LN, LE, V]

object GraphBuilderEquationSystem:
  /**
   * Returns the standard implementation of a finite equation system.
   * @see
   *   [[SimpleGraphBuilderEquationSystem]] for the meaning of all the
   *   parameters.
   */

  def apply[LN, LE, V](
      builder: GraphBodyBuilder[LN, LE, V],
      combiner: (V, V) => V,
      inputUnknowns: Iterable[builder.U]
  ): SimpleGraphBuilderEquationSystem[LN, LE, V] =
    SimpleGraphBuilderEquationSystem(builder, combiner, inputUnknowns)
