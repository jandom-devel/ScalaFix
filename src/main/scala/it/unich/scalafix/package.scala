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

package it.unich

import it.unich.scalafix.utils.IterableFunction

import scala.language.implicitConversions

/**
  * The fixpoint package contains everything which is related to defining and solving systems
  * of equations. This package object defines some type aliases which are used in the API.
  */
package object scalafix {
  /**
    * An assignment for an equation system is a map from unknowns to values.
    */
  type Assignment[U, V] = U => V

  /**
    * A partial assignment for an equation system is an iterable function from unknowns to
    * values.
    */
  type PartialAssignment[U, V] = IterableFunction[U, V]
}
