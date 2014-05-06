package cspom.xcsp;

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import cspom.CSPOM
import cspom.compiler.ProblemCompiler

final class ParserTest extends FlatSpec with Matchers {
  val FILENAME = "crossword-m1-debug-05-01.xml";

  "XCSPParser" should s"parse $FILENAME" in {

    val cspom = XCSPParser.parse(classOf[ParserTest].getResourceAsStream(FILENAME))._1;
    //println(cspom);
    cspom.namedExpressions should have size 25
    assert(!cspom.namedExpressions.values.exists(_.params.contains("var_is_introduced")))

    cspom.constraints.size should be >= 55
  }
}
