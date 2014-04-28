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
import org.scalatest.FlatSpec

class FlatZinc extends FlatSpec {

  val url = classOf[FlatZinc].getResource("1d_rubiks_cube.fzn")

  "FlatZinc parser" should s"parse $url" in {
    val (cspom, variables) = FlatZincParser.parse(url.openStream)
  }

}

