package cspom.flatzinc

import java.text.ParseException
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.StreamReader
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers._
import java.io.StringReader
import cspom.CSPOM

class FlatZinc {

  @Test
  def test() {
    val url = io.Source.fromURL(classOf[FlatZinc].getResource("1d_rubiks_cube.fzn"))
    val in = StreamReader(url.reader)
    //val tokens = new FlatzincDSL.lexical.Scanner(in)

    val r = FlatzincDSL.flatzincModel(in) //FlatzincDSL.flatzincModel)(tokens)

    println(r.get)
    //    r match {
    //      case Success(list, msg) => println(list)
    //      case e: NoSuccess => throw new IllegalArgumentException()
    //    }
  }

}
