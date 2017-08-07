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

package it.unich.scalafix

/**
  * The `FixpointSolver` object contains many enumerations and auxiliary classes which are used by
  * other fixpoint solvers.
  */
object FixpointSolver {

  /**
    * This exception is thrown when the parameters provided to the `Driver` are not correct.
    */
  class DriverBadParameters(msg: String) extends Exception(msg)

  /**
    * An enumeration with the solvers supported by this driver.
    */
  object Solver extends Enumeration {
    type Solver = Value

    val KleeneSolver = Value
    val RoundRobinSolver = Value
    val PriorityWorkListSolver = Value
    val WorkListSolver = Value
    val HierarchicalOrderingSolver = Value
  }

  object BoxStrategy extends Enumeration {
    type BoxStrategy = Value

    /**
      * Only apply widenings.
      */
    val OnlyWidening = Value

    /**
      * Standard two pass widenings/narrowings iteration.
      */
    val TwoPhases = Value

    /**
      * Single pass with a warrowing.
      */
    val Warrowing = Value
  }

  object BoxScope extends Enumeration {
    type BoxScope = Value

    /**
      * Use standard widenings.
      */
    val Standard = Value

    /**
      * Use localized widenings.
      */
    val Localized = Value
  }

  object BoxLocation extends Enumeration {
    type BoxLocation = Value

    /**
      * Put widenings/narrowings points nowhere
      */
    val None = Value

    /**
      * Put widenings/narrowings points at each unknown.
      */
    val All = Value

    /**
      * Put widenings/narrowings points at each loop head.
      */
    val Loop = Value
  }

  object RestartStrategy extends Enumeration {
    type RestartStrategy = Value

    /**
      * Do not apply a restart strategy
      */
    val None = Value

    /**
      * Apply a restart strategy
      */
    val Restart = Value
  }
}
