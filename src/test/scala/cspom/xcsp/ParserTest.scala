package cspom.xcsp;

import scala.collection.JavaConversions
import cspom.CSPOM
import org.junit.Assert._
import org.junit.Test

final class ParserTest {
  val FILENAME = "crossword-m1-debug-05-01.xml";

  @Test
  def test() {
    val cspom = new CSPOM();
    new XCSPParser(cspom).parse(classOf[ParserTest].getResourceAsStream(FILENAME));
    //println(cspom);
    assertEquals(25, cspom.variables.filterNot(_.params.contains("var_is_introduced")).size)
    assertTrue(cspom.constraints.size >= 55)
    println(cspom)
  }

}
