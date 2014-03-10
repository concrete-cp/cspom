package cspom.variable

import org.junit.Assert._
import org.junit.Test
import cspom.xcsp.XCSPParser
class IntDomainTest {
  @Test
  def testValueOf() {
    assertEquals(
      IntDomain(Seq(1, 3, 4, 5, 6, 7, 8, 9, 10, 18, 19, 20)),
      XCSPParser.parseDomain("1 3..10 18..20"))
    assertEquals(new IntInterval(3, 10), XCSPParser.parseItv("3..10"))
    assertTrue(XCSPParser.parseItv("3..10").isInstanceOf[IntInterval])
    assertEquals(new IntInterval(3, 10), XCSPParser.parseDomain("3..5 6 7..10"))
    assertTrue(XCSPParser.parseDomain("3..5 6 7..10").isInstanceOf[IntInterval])
  }
}