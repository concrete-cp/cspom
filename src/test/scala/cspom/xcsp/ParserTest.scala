package cspom.xcsp;

import cspom.CSPOM
import org.junit.Assert._
import org.junit.Test

final class ParserTest {
  val FILENAME = "crossword-m1-debug-05-01.xml";

  @Test
  def test() {
    val cspom = new CSPOM();
    val parser = new XCSPParser(cspom);
    parser.parse(classOf[ParserTest].getResourceAsStream(FILENAME));
    assertEquals(25, cspom.variables.size)
    assertEquals(55, cspom.constraints.size)
    //System.out.println(cspom);
  }

}
