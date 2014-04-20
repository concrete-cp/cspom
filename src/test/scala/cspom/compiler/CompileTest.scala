package cspom.compiler;

import cspom.xcsp.{ ParserTest, XCSPParser }
import cspom.CSPOM
import org.scalatest.FlatSpec

final class CompileTest extends FlatSpec {
  val FILENAME = "crossword-m1-debug-05-01.xml";

  "ProblemCompiler" should s"compile $FILENAME" in {
    val cspom = XCSPParser.parse(classOf[ParserTest].getResourceAsStream(FILENAME))._1;
    ProblemCompiler.compile(cspom, Seq())
  }

}
