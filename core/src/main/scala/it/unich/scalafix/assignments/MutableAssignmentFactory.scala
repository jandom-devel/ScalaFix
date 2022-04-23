/**
 * Copyright 2021 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.scalafix.*

/**
 * This is a factory which builds mutable assignments a simple assignment.
 * Although some generic factories are provided in the library, other factories
 * may be build for specialized use.
 *
 * @tparam U
 *   type for unknowns
 * @tparam V
 *   type for values
 * @tparam X
 *   type for the mutable assignment
 */
trait MutableAssignmentFactory[U, V]:
  def apply(rho: Assignment[U, V]): MutableAssignment[U, V]

given defaultMutableAssignmentFactory[U, V]: MutableAssignmentFactory[U, V] = MutableAssignment(_)
