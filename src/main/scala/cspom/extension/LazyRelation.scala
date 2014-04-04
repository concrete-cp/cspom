package cspom.extension

class LazyRelation[A](val f: Unit => Relation[A]) extends Relation[A] {

  lazy val apply = f(Unit)

  def iterator = apply.iterator

  def contains(t: Seq[A]) = apply.contains(t)

  def filter(filt: (Int, A) => Boolean) = new LazyRelation(f.andThen(_.filter(filt)))

  def project(c: Seq[Int]) = new LazyRelation(f.andThen(_.project(c)))

  override def toString = "Lazy relation: " + apply.toString
}