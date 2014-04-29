package cspom.flatzinc

import scala.util.parsing.combinator.JavaTokenParsers

import org.scalatest.FlatSpec

import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ProblemCompiler

class FlatZinc extends FlatSpec {

  val url = classOf[FlatZinc].getResource("1d_rubiks_cube.fzn")

  "FlatZinc parser" should s"parse $url" in {
    val (cspom, variables) = FlatZincParser.parse(url.openStream)
  }

}

