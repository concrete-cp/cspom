package cspom.extension

import scala.collection.Seq

class Table[A](val table: Seq[Seq[A]]) extends Relation[A] {
  // Members declared in scala.collection.IterableLike
  def iterator: Iterator[Seq[A]] = table.iterator
  // Members declared in cspom.extension.Relation
  def arity: Int = table.head.length
  def contains(t: Seq[A]): Boolean = table.exists(_ sameElements t)
  def filter(f: (Int, A) => Boolean) = {
    new Table(table.filter(t => t.zipWithIndex.forall { case (v, k) => f(k, v) }))
  }

  override def toString = "Table(\n  " + table.map(_.mkString(", ")).mkString("\n  ") + ")"

  def project(c: Seq[Int]) = new Table(table.map(t => c.map(t)).distinct)
}