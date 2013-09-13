package cspom.variable

import org.junit.Assert._
import org.junit.Test
class IntDomainTest {
  @Test
  def testValueOf() {
    assertEquals(
      IntDomain.of(1, 3, 4, 5, 6, 7, 8, 9, 10, 18, 19, 20),
      IntDomain.valueOf("1 3..10 18..20"))
    assertEquals(new IntInterval(3, 10), IntDomain.valueOf("3..10"))
    assertTrue(IntDomain.valueOf("3..10").isInstanceOf[IntInterval])
    assertEquals(new IntInterval(3, 10), IntDomain.valueOf("3..5 6 7..10"))
    assertTrue(IntDomain.valueOf("3..5 6 7..10").isInstanceOf[IntInterval])
  }
}