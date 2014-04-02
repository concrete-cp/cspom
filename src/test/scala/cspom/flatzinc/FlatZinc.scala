package cspom.flatzinc

import java.text.ParseException
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.StreamReader
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers._
import java.io.StringReader
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ProblemCompiler

class FlatZinc {

  @Test
  def test() {
    val url = classOf[FlatZinc].getResource("1d_rubiks_cube.fzn")

    //val tokens = new FlatzincDSL.lexical.Scanner(in)

    val (cspom, variables) = FlatZincParser.parse(url.openStream) //FlatzincDSL.flatzincModel)(tokens)

//
//    println(cspom)
//    
//    println(variables)

    //    r match {
    //      case Success(list, msg) => println(list)
    //      case e: NoSuccess => throw new IllegalArgumentException()
    //    }
  }

}

