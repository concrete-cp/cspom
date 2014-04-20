package cspom.extension

trait Relation[A] extends Set[Seq[A]] {

  def tupleString = map { _.mkString(" ") } mkString "|"

  def contains(t: Seq[A]): Boolean

  def filter(f: (Int, A) => Boolean): Relation[A]
  
  def project(c: Seq[Int]): Relation[A]

}