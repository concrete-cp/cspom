package cspom.extension

import cspom.Loggable
import java.util.StringTokenizer
import scala.xml.Node
import java.io.StringReader

object LazyRelation {
  var id = 0
}

final class LazyRelation(private val text: StringReader, val arity: Int, private val nbTuples: Int) extends Relation with Loggable {

  private def traversable = new Traversable[Array[Int]] {
    def foreach[A](f: Array[Int] => A) {
      text.reset()
      val buf = new Array[Char](bl)
      var nt = 0
      var b = new StringBuilder()
      while (true) {
        val l = text.read(buf, 0, bl)
        if (l < 0) {
          if (b.nonEmpty) {
            f(b.toString.trim.split(" +").map(_.toInt))
            nt += 1
          }
          require(nt == nbTuples, nt + " parsed, " + nbTuples + " declared")
          return
        }

        var i = 0
        while (i < l) {
          if (buf(i) == '|') {
            f(b.toString().trim.split(" +").map(_.toInt))
            nt += 1
            b.delete(0, b.length)
          } else {
            b.append(buf(i))
          }
          i += 1
        }
      }

    }
  }

  val bl = 4096

  val id = LazyRelation.id
  LazyRelation.id += 1

  def iterator = traversable.toStream.iterator

  override def isEmpty = nbTuples == 0

  override def toString = id.toString

  override def hashCode = id

  override def equals(o: Any) = o match {
    case a: AnyRef => this eq a
    case _ => false
  }

  //  
  //  new Iterator[Array[Int]] {
  //    val st = new StringTokenizer(text, "|")
  //    var c = 0
  //    def hasNext = {
  //      if (st.hasMoreTokens) true
  //      else {
  //        require(c == nbTuples)
  //        text = null
  //        false
  //      }
  //    }
  //    def next() = {
  //      c += 1
  //      val t = st.nextToken().trim.split(" +")
  //      require(t.length == arity, t.toSeq.toString)
  //      t.map(_.toInt)
  //    }
  //  }

  def contains(t: Seq[Int]) = exists(_ sameElements t)

}