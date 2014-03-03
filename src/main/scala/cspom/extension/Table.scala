package cspom.extension

import scala.collection.Seq

class Table(val table: Seq[Seq[Int]]) extends Relation {
  // Members declared in scala.collection.IterableLike
  def iterator: Iterator[Seq[Int]] = table.iterator
  // Members declared in cspom.extension.Relation
  def arity: Int = table.head.length
  def contains(t: Seq[Int]): Boolean = table.exists(_ sameElements t)
  def filter(f: (Int, Int) => Boolean): Relation = {
    new Table(table.filter(t => t.zipWithIndex.forall { case (v, k) => f(k, v) }))
  }

  override def toString = "Table(\n  " + table.map(_.mkString(", ")).mkString("\n  ") + ")"

  def project(c: Seq[Int]) = new Table(table.map(t => c.map(t)).distinct)
}