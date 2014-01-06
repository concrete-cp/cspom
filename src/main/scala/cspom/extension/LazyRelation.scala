package cspom.extension

import cspom.Loggable
import java.util.StringTokenizer
import scala.xml.Node
import java.io.StringReader
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Reader
import java.io.BufferedReader

object LazyRelation {
  var id = 0
}

final class LazyRelation(private val reader: Reader[Char], val arity: Int, override val size: Int) extends Relation with Loggable {

  private object Parser extends RegexParsers {
    def parse: Parser[Seq[Array[Int]]] = repsep(tuple, "|")
    def tuple: Parser[Array[Int]] = rep1(wholeNumber) ^^ (_.toArray)
    def wholeNumber: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
  }

  val id = LazyRelation.id
  LazyRelation.id += 1

  //  def iterator = {
  //    val seq = text.toString.split("|").map(_.split(" ").map(_.toInt)) //Parser.parse(text).get
  //    require(seq.size == size, seq.size + " parsed, " + size + " declared")
  //    seq.iterator
  //  }

  override def toString = s"relation #${id} with $size tuples"

  override def hashCode = id

  override def equals(o: Any) = o match {
    case a: AnyRef => this eq a
    case _ => false
  }

  //  

  @annotation.tailrec
  private def readerToString(r: Reader[Char], stb: StringBuilder = new StringBuilder): String =
    if (r.atEnd) {
      stb.toString
    } else {
      readerToString(r.rest, stb.append(r.first))
    }

  def iterator = new Iterator[Array[Int]] {
    val text = readerToString(reader)

    val st = new StringTokenizer(text, "|")

    def hasNext = st.hasMoreTokens
    def next() = {
      val t = st.nextToken().trim.split(" +")
      require(t.length == arity, t.toSeq.toString)
      t.map(_.toInt)
    }
  }

  def contains(t: Seq[Int]) = exists(_ sameElements t)

}