/**
  * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.drivers

import it.unich.scalafix._
import it.unich.scalafix.drivers.Driver._
import it.unich.scalafix.finite._
import it.unich.scalafix.lattice._
import it.unich.scalafix.structured.GraphBasedEquationSystem
import it.unich.scalafix.utils.PMaps._

/**
  * This driver is an interface for solvers of layered equation systems.
  */
object StructuredDriver extends Driver {
  /**
    * Returns an assignment of a widening for each unknown.
    *
    * @param w input parameter which drives the generation of the widening assignment.
    */
  private def wideningDefine[V](w: Widenings.Widening): Box[V] = {
    w match {
      case Widenings.None => Box.right[V]
      case Widenings.Delayed(first, delay, next) => Box.cascade(wideningDefine[V](first), delay, wideningDefine[V](next))
    }
  }

  /**
    * Returns an assignment of a narrowing for each unknown.
    *
    * @param n input parameter which drives the generation of the narrowing assignment.
    */
  private def narrowingDefine[V](n: Narrowings.Narrowing): Box[V] = {
    n match {
      case Narrowings.Stop => Box.left[V]
      case Narrowings.None => Box.right[V]
      case Narrowings.Delayed(first, delay, next) => Box.cascade(narrowingDefine[V](first), delay, narrowingDefine[V](next))
    }
  }

  /**
    * Returns an assignment of a mixed box for each unknown.
    *
    * @param u input parameter which drives the generation of the mixed assignment.
    */
  private def warrowingDefine[V <: PartiallyOrdered[V]](u: Updates.Update): Box[V] = {
    u match {
      case Updates.Combine(widening, narrowing) => Box.warrowing(wideningDefine[V](widening), narrowingDefine[V](narrowing))
    }
  }

  /**
    * Given an equation system and a box assignment, filter the assignment according to what specified in the input parameter location and
    * the graph ordering.
    *
    * @param eqs      an equation system
    * @param boxes    a box assignment
    * @param location input parameter which drives the filtering by specifying where to put boxes.
    * @param ordering a GraphOrdering used when we need to detect heads.
    */
  private def boxFilter[U, V](eqs: FiniteEquationSystem[U, V], boxes: Box[V], location: BoxLocation.Value, ordering: Option[GraphOrdering[U]]): Option[BoxAssignment[U, V]] =
    location match {
      case BoxLocation.None => None
      case BoxLocation.All => Some(boxes)
      case BoxLocation.Loop => Some(BoxAssignment(boxes).restrict(ordering.get.isHead))
    }

  /**
    * Apply a given box assignment to an equation system, generatig a new equation system.
    *
    * @param eqs      the equation system
    * @param optBoxes an optional box assignment
    * @param scope    an input parameters which determines how we want to apply boxes (such as localized or standard)
    * @param ordering an optional ordering on unknowns to be used for localized boxes.
    */
  private def boxApply[U, V, E](eqs: GraphBasedEquationSystem[U, V, E], optBoxes: Option[BoxAssignment[U, V]], scope: BoxScope.Value, ordering: Option[Ordering[U]]): FiniteEquationSystem[U, V] = {
    if (optBoxes.isEmpty)
      eqs
    else scope match {
      case BoxScope.Standard => eqs.withBoxes(optBoxes.get)
      case BoxScope.Localized => eqs.withLocalizedBoxes(optBoxes.get, ordering.get)
    }
  }

  def addLocalizedBoxes[U, V <: DirectedPartiallyOrdered[V], E](eqs: GraphBasedEquationSystem[U, V, E], widening: BoxAssignment[U, V], narrowing: BoxAssignment[U, V], ordering: Ordering[U]): FiniteEquationSystem[U, V] = {
    val ingoing = eqs.targets.inverse
    val newbody = new Body[U, V] {
      def apply(rho: Assignment[U, V]) = {
        (x: U) =>
          val contributions = for (e <- ingoing.image(x)) yield {
            val contrib = eqs.edgeBody(e)(rho)(x)
            val boxapply = eqs.sources.image(e).exists {
              ordering.lteq(x, _)
            } && !(contrib <= rho(x))
            (contrib, boxapply)
          }
          // if contribution is empty the unknown x has no right hand side... it seems
          // reasonable to return the old value.
          if (contributions.isEmpty)
            rho(x)
          else {
            val result = contributions reduce { (x: (V, Boolean), y: (V, Boolean)) => (x._1 upperBound y._1, x._2 || y._2) }
            //println((x, rho(x), contributions))
            if (result._2) {
              widening(x)(rho(x), result._1)
            } else if (result._1 < rho(x)) narrowing(x)(rho(x), result._1) else result._1
          }
      }
    }

    FiniteEquationSystem(
      body = newbody,
      inputUnknowns = eqs.inputUnknowns,
      unknowns = eqs.unknowns,
      infl = if (widening.areIdempotent && narrowing.areIdempotent) eqs.infl else eqs.infl.withDiagonal
    )
  }

  /**
    * Solves the equation system using the parameters specified in p.
    *
    * @param eqs the equation system to solve
    * @param p   parameters passed through a PMap
    */
  def apply[U, V <: DirectedPartiallyOrdered[V], E](eqs: GraphBasedEquationSystem[U, V, E], p: PNil): U => V = {
    val solver = p(Driver.solver)
    val boxlocation = p(Driver.boxlocation)
    val boxstrategy = p(Driver.boxstrategy)
    val boxscope = p(Driver.boxscope)
    val restartstrategy = p(Driver.restartstrategy)

    val ordering1: Option[GraphOrdering[U]] = (solver, boxscope) match {
      case (Solver.HierarchicalOrderingSolver, _) =>
        Some(HierarchicalOrdering(DFOrdering(eqs)))
      case (Solver.PriorityWorkListSolver, _) | (_, BoxScope.Localized) =>
        Some(DFOrdering(eqs))
      case _ =>
        None
    }

    val ordering: Option[GraphOrdering[U]] = boxlocation match {
      case BoxLocation.None | BoxLocation.All =>
        None
      case BoxLocation.Loop =>
        ordering1 orElse Some(DFOrdering(eqs))
    }

    val restart: (V, V) => Boolean =
      if (restartstrategy) { (x, y) => x < y }
      else { (x, y) => false }

    boxstrategy match {
      case BoxStrategy.OnlyWidening =>
        val widening = boxFilter[U, V](eqs, wideningDefine[V](p(Driver.widening)), boxlocation, ordering)
        val withWidening = boxApply(eqs, widening, boxscope, ordering)
        FiniteDriver(withWidening, eqs.initial, ordering, restart, p)
      case BoxStrategy.TwoPhases =>
        val widening = boxFilter[U, V](eqs, wideningDefine(p(Driver.widening)), boxlocation, ordering)
        val withWidening = boxApply(eqs, widening, boxscope, ordering)
        val ascendingAssignment = FiniteDriver(withWidening, eqs.initial, ordering, restart, p)
        val narrowing = boxFilter[U, V](eqs, narrowingDefine(p(Driver.narrowing)), boxlocation, ordering)
        // localizing narrowing does not seem useful
        val withNarrowing = boxApply(eqs, narrowing, BoxScope.Standard, ordering)
        FiniteDriver(withNarrowing, ascendingAssignment, ordering, restart, p)
      case BoxStrategy.Mixed =>
        if (boxscope == BoxScope.Localized) {
          val widening = boxFilter[U, V](eqs, wideningDefine(p(Driver.widening)), boxlocation, ordering)
          val narrowing = boxFilter[U, V](eqs, narrowingDefine(p(Driver.narrowing)), boxlocation, ordering)
          val withUpdate = if (widening.isEmpty && narrowing.isEmpty)
            eqs
          else
            addLocalizedBoxes(eqs, widening getOrElse BoxAssignment.empty[V], narrowing getOrElse BoxAssignment.empty[V], ordering.get)
          FiniteDriver(withUpdate, eqs.initial, ordering, restart, p)
        } else {
          val warrowingAssignment = boxFilter[U, V](eqs, warrowingDefine(p(Driver.update)), boxlocation, ordering)
          val eqsWithWarrowing = boxApply(eqs, warrowingAssignment, boxscope, ordering)
          FiniteDriver(eqsWithWarrowing, eqs.initial, ordering, restart, p)
        }
    }
  }
}
