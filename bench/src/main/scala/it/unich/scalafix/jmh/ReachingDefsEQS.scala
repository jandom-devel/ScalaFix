/**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it>
 *                   and Francesca Scozzari <francesca.scozzari@unich.it>
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
import it.unich.scalafix.finite.*
import it.unich.scalafix.graphs.*
import it.unich.scalafix.utils.*
import scala.compiletime.ops.boolean

/**
 * This object contains many implementations of an equation system for reaching
 * definition analysis of the following three-address code program:
 * ```
 * d1 --> i = m-1;
 * d2 --> j = n;
 * d3 --> a = u1;
 * do
 *   d4 --> i = i+1;
 *   d5 --> j = j-1;
 *   if (e1) then
 *     d6 --> a = u2;
 *   else
 *     d7 --> i = u3
 * while (e2)
 * ```
 * The example comes from: <blockquote> Alfred V. Aho, Ravi Sethi, Jeffrey D.
 * Ullman.<br> <em>Compilers. Principles, Techniques, and Tools</em><br>
 * Addison-Wesley Publishing Company, 1986 </blockquote>
 */
object ReachingDefsEQS:

  /** First unknown of the equation system. */
  val firstUnknown = 1

  /** Last unknown of the equation system. */
  val lastUnknown = 7

  /** Returns a finite equation system. */
  def createFiniteEQS() = FiniteEquationSystem[Int, Set[Int]](
    initialBody = (rho: Int => Set[Int]) => {
      case 1 => Set(1) -- Set(4, 7)
      case 2 => Set(2) ++ (rho(1) -- Set(5))
      case 3 => Set(3) ++ (rho(2) -- Set(6))
      case 4 => Set(4) ++ (rho(3) ++ rho(6) ++ rho(7) -- Set(1, 7))
      case 5 => Set(5) ++ (rho(4) -- Set(2))
      case 6 => Set(6) ++ (rho(5) -- Set(3))
      case 7 => Set(7) ++ (rho(5) -- Set(1, 4))
    },
    initialInfl = Relation(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6, 5 -> 7, 6 -> 4, 7 -> 4),
    unknowns = firstUnknown to lastUnknown,
    inputUnknowns = Seq(1)
  )

  /** Returns a graph-based equation system. */
  def createGraphEQS() =
    val graphBody = GraphBody[(Int, Boolean), Set[Int], String](
      sources = Relation(
        "1to2" -> (1, false),
        "2to3" -> (2, false),
        "3to4'" -> (3, false),
        "6to4'" -> (3, false),
        "7to4'" -> (3, false),
        "4'to4" -> (4, true),
        "4to5" -> (4, false),
        "5to6" -> (5, false),
        "5to7" -> (5, false)
      ),
      target = Map(
        "*to1" -> (1, false),
        "1to2" -> (2, false),
        "2to3" -> (3, false),
        "3to4'" -> (4, true),
        "6to4'" -> (4, true),
        "7to4'" -> (4, true),
        "4'to4" -> (4, false),
        "4to5" -> (5, false),
        "5to6" -> (6, false),
        "5to7" -> (7, false)
      ),
      ingoing = Relation(
        (1, false) -> "*to1",
        (2, false) -> "1to2",
        (3, false) -> "2to3",
        (4, true) -> "3to4'",
        (4, true) -> "6to4'",
        (4, true) -> "7to4'",
        (4, false) -> "4'to4",
        (5, false) -> "4to5",
        (6, false) -> "5to6",
        (7, false) -> "5to7"
      ),
      outgoing = Relation(
        (1, false) -> "1to2",
        (2, false) -> "2to3",
        (3, false) -> "3to4'",
        (4, true) -> "4'to4",
        (4, false) -> "4to5",
        (5, false) -> "5to6",
        (5, false) -> "5to7",
        (6, false) -> "6to4'",
        (7, false) -> "7to4'"
      ),
      edgeAction = (rho: Assignment[(Int, Boolean), Set[Int]]) => {
        case "*to1"  => Set(1) -- Set(4, 7)
        case "1to2"  => Set(2) ++ (rho(1, false) -- Set(5))
        case "2to3"  => Set(3) ++ (rho(2, false) -- Set(6))
        case "3to4'" => rho(3, false)
        case "6to4'" => rho(6, false)
        case "7to4'" => rho(7, false)
        case "4'to4" => Set(4) ++ (rho(4, true) -- Set(1, 7))
        case "4to5"  => Set(5) ++ (rho(4, false) -- Set(2))
        case "5to6"  => Set(6) ++ (rho(5, false) -- Set(3))
        case "5to7"  => Set(7) ++ (rho(5, false) -- Set(1, 4))
      },
      unknowns = ((firstUnknown to lastUnknown) map { (i: Int) => (i, false) }) ++ Seq((4, true)),
      combiner = _ ++ _
    )

    GraphEquationSystem(
      initialGraph = graphBody,
      inputUnknowns = Seq((1, false))
    )

  object ReachingDefsGraphBodyBuilder extends GraphBodyBuilder[String, String, Set[Int]]:
    val n1 = addNode("1")
    val n2 = addNode("2")
    val n3 = addNode("3")
    val n4 = addNode("4")
    val n4p = addNode("4'")
    val n5 = addNode("5")
    val n6 = addNode("6")
    val n7 = addNode("7")
    addEdge("*to1", Seq(), n1, _ => Set(1) -- Set(4, 7))
    addEdge("1to2", Seq(n1), n2, rho => Set(2) ++ (rho(n1) -- Set(5)))
    addEdge("2to3", Seq(n2), n3, rho => Set(3) ++ (rho(n2) -- Set(6)))
    addEdge("3to4'", Seq(n3), n4p, _(n3))
    addEdge("6to4'", Seq(n6), n4p, _(n6))
    addEdge("7to4'", Seq(n7), n4p, _(n7))
    addEdge("4'to4", Seq(n4p), n4, (rho) => Set(4) ++ (rho(n4p) -- Set(1, 7)))
    addEdge("4to5", Seq(n4), n5, rho => Set(5) ++ (rho(n4) -- Set(2)))
    addEdge("5to6", Seq(n5), n6, rho => Set(6) ++ (rho(n5) -- Set(3)))
    addEdge("5to7", Seq(n5), n7, rho => Set(7) ++ (rho(n5) -- Set(1, 4)))

  /**
   * Returns a graph-based equation system construed over a graph body builder.
   */
  def createGraphBuilderEQS() = GraphBuilderEquationSystem(
    ReachingDefsGraphBodyBuilder,
    _ ++ _,
    Seq(ReachingDefsGraphBodyBuilder.n1)
  )

  /**
   * Validates an asssignment for the equation system returned by
   * [[createGraphBuilderEQS]].
   */
  def validateGraphBuilderAssignment(
      rho: Assignment[ReachingDefsGraphBodyBuilder.U, Set[Int]]
  ): Unit =
    import ReachingDefsGraphBodyBuilder.*
    assert(rho(n1) == Set(1))
    assert(rho(n2) == Set(1, 2))
    assert(rho(n3) == Set(1, 2, 3))
    assert(rho(n4) == Set(2, 3, 4, 5, 6))
    assert(rho(n5) == Set(3, 4, 5, 6))
    assert(rho(n6) == Set(4, 5, 6))
    assert(rho(n7) == Set(3, 5, 6, 7))
