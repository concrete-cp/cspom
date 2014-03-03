package cspom.extension

class LazyMDD(val f: Unit => MDD) extends Relation {

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

  def contains(t: Seq[Int]) = apply.contains(t)

  def filter(filt: (Int, Int) => Boolean) = new LazyMDD(f.andThen(_.filter(filt)))

  def project(c: Seq[Int]) = new LazyMDD(f.andThen(_.project(c)))
}