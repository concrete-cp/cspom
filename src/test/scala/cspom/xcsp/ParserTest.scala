package cspom.xcsp;

import cspom.CSPOM
import org.junit.Assert._
import org.junit.Test

final class ParserTest {

  @Test
  def test() {
    val cspom = new CSPOM();
    val parser = new XCSPParser(cspom);
//    parser.parse(classOf[ParserTest].getResourceAsStream(ParserTest.FILENAME));
    //System.out.println(cspom);
  }

}

object ParserTest {
  val FILENAME = "crossword-m1-debug-05-01.xml";
}
