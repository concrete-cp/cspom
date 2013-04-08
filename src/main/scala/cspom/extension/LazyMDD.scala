package cspom.extension

class LazyMDD(val f: Unit => MDD) extends Relation {

  lazy val apply = f()

  def iterator = apply.iterator

  def arity = apply.arity

  def contains(t: Seq[Int]) = apply.contains(t)
}