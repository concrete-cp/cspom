package cspom.compiler;

import cspom.xcsp.{ParserTest, XCSPParser}
import cspom.CSPOM
import org.junit.Test

final class CompileTest {
  val FILENAME = "crossword-m1-debug-05-01.xml";

  @Test
  def test() {
    val cspom = new CSPOM();
    new XCSPParser(cspom).parse(classOf[ParserTest].getResourceAsStream(FILENAME));
    ProblemCompiler.compile(cspom, Seq())
    //println(cspom);

  }

}
