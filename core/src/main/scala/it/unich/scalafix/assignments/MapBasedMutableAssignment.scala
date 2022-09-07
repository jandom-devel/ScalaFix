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

package it.unich.scalafix.assignments

import it.unich.scalafix.Assignment
import it.unich.scalafix.MutableAssignment

import scala.collection.MapFactory
import scala.collection.mutable

/**
 * A mutable assignment backed by a mutable map.
 *
 * @param rho
 *   the initial value of this assignment.
 * @param factory
 *   a factory for maps. Changing this parameter it is possible to choose which
 *   implementation of mutable maps we want to use.
 */
class MapBasedMutableAssignment[U, V](
    rho: Assignment[U, V],
    factory: MapFactory[mutable.Map] = mutable.Map
) extends MutableAssignment[U, V]:

  private val m = factory.empty.withDefault(rho)

  export m.{apply, update, isDefinedAt, iterator, keys as unknowns}

  /**
   * @inheritdoc
   *
   * The string representation is human-readable, and shows both the updated
   * binding of the assignment and the initial assignment.
   */
  override def toString: String = s"${m.mkString("[ ", ", ", " ]")} over $rho"
