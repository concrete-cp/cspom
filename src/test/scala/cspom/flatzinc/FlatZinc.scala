package cspom.flatzinc

import scala.util.parsing.combinator.JavaTokenParsers
import org.scalatest.FlatSpec
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.compiler.CSPOMCompiler
import org.scalatest.TryValues

class FlatZinc extends FlatSpec with TryValues {

  val url = classOf[FlatZinc].getResource("1d_rubiks_cube.fzn")

  "FlatZinc parser" should s"parse $url" in {
    val (cspom, variables) = FlatZincParser(url.openStream).success.get
  }

}

