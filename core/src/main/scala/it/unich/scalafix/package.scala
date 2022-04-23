/**
 * Copyright 2015, 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
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

/**
 * The fixpoint package contains everything which is related to defining and
 * solving systems of equations.
 */
package it.unich.scalafix

/**
 * An assignment is a map from unknowns to values.
 *
 * @tparam U
 *   type for unknowns
 * @tparam V
 *   type for values
 */
type Assignment[-U, +V] = U => V

/**
 * The constant to use in -Xelide-below in order to remove tracing code. Note
 * that tracing might be required to make a program work, so only elide it if
 * you known what you are doing.
 */
final val TRACING = 5000
