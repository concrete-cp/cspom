package cspom.extension

import scala.collection.mutable.HashSet

/**
 * CSPFJ Competitor - CSP solver using the CSPFJ API for Java
 * Copyright (C) 2006 Julien VION
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA  02110-1301  USA
 */

/**
 * This class is used to represent relations, that is a set of tuples.
 *
 *
 * @author vion
 *
 */
final class Relation(val arity: Int) extends HashSet[Array[_ <: Any]] {

  def tupleString = iterator map { _.mkString(" ") } mkString "|"

  override def toString = arity + "-ary, " + size + " tuples"

  def addTuple(tuple: Any*): Boolean = {
    assume(tuple.size == arity, tuple.toString + "'s arity should be " + arity)
    add(tuple.toArray)
  }

  def containsTuple(tuple: Any*) = contains(tuple.toArray)

  /**
   * This method returns a copy of this extension with permuted tuples. New
   * order of tuples is given as an argument.
   *
   * <p>
   * For example, a ternary extension 1 2 3|1 3 4|2 4 5 will be reversed to 1
   * 3 2|1 4 3|2 5 4 by a call to reverse(0, 2, 1).
   * </p>
   *
   * @param newOrder
   *            new order of the extension.
   * @return a reversed copy of the extension.
   */
  def permute(newOrder: Seq[Int]): Relation =
    new Relation(arity) ++= (this map { t => newOrder map { i => t(i) } toArray })

}
