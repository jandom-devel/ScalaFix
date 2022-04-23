/**
 * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.scalafix.lattice

/**
 * A magma is a set with a binary operation. The Magma trait is a type class for
 * magmas, which defines a single binary operation called `op`.
 */
trait Magma[A]:
  /** The magma  operation. */
  extension (x: A) infix def op(y: A): A

/**
 * A magma obtained by a domain, taking the upperBound as the magma operation.
 */
given domainIsMagma[A: Domain]: Magma[A] with
  extension (x: A) def op(y: A): A = x upperBound y
