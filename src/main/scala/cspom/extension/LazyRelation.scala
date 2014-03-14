package cspom.extension

class LazyRelation[A](val f: Unit => Relation[A]) extends Relation[A] {

  var _arity = -1

  def apply = {
    val m = f()
    _arity = m.arity

    m
  }

  def iterator = apply.iterator

  def arity = {
    if (_arity < 0) {
      apply.arity
    } else {
      _arity
    }
  }

  def contains(t: Seq[A]) = apply.contains(t)

  def filter(filt: (Int, A) => Boolean) = new LazyRelation(f.andThen(_.filter(filt)))

  def project(c: Seq[Int]) = new LazyRelation(f.andThen(_.project(c)))
}