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
    assertEquals(25, JavaConversions.collectionAsScalaIterable(cspom.variables).filter(!_.auxiliary).size)
    assertTrue(cspom.constraints.size >= 55)
  }

}
