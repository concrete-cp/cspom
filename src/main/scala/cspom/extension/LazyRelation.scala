package cspom.extension

import cspom.Loggable
import java.util.StringTokenizer
import scala.xml.Node
import java.io.StringReader

final class LazyRelation(private val text: StringReader, val arity: Int, private val nbTuples: Int) extends Relation with Loggable {

  val bl = 4096

  def foreach[A](f: Array[Int] => A) {
    val buf = new Array[Char](bl)
    var nt = 0
    var b = new StringBuilder()
    while (true) {
      val l = text.read(buf, 0, bl)
      if (l < 0) {
        text.close()
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
  
  override def isEmpty = nbTuples == 0

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

  def contains(t: Seq[_]) = exists(_ sameElements t)

}