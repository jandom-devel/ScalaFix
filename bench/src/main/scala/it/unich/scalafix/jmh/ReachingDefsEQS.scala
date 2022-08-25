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
import it.unich.scalafix.finite.*
import it.unich.scalafix.graphs.*
import it.unich.scalafix.utils.*

/**
 * This object contain many implementations of an equation system for reaching
 * definition analisys of the folling three-address code program:
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
      case 3 => Set(3) ++ (rho(2) -- Set(7))
      case 4 => Set(4) ++ (rho(3) ++ rho(7) ++ rho(6) -- Set(1, 7))
      case 5 => Set(5) ++ (rho(4) -- Set(2))
      case 6 => Set(6) ++ (rho(5) -- Set(3))
      case 7 => Set(7) ++ (rho(5) -- Set(1, 4))
    },
    initialInfl = Relation(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6, 5 -> 7, 6 -> 4, 7 -> 4),
    unknowns = firstUnknown to lastUnknown,
    inputUnknowns = Seq(1)
  )

  /** Retuens a graph-based equation system. */
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
