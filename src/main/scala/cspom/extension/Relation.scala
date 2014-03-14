package cspom.extension

trait Relation[A] extends Iterable[Seq[A]] {

  def tupleString = map { _.mkString(" ") } mkString "|"

  def contains(t: Seq[A]): Boolean

  def arity: Int

  def filter(f: (Int, A) => Boolean): Relation[A]
  
  def project(c: Seq[Int]): Relation[A]

}