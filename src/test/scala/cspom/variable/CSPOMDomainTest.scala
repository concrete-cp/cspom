package cspom.variable

import org.junit.Assert._
import org.junit.Test
class CSPOMDomainTest {
  @Test
  def testValueOf() {
    assertEquals(
      ExtensiveDomain.of(1, 3, 4, 5, 6, 7, 8, 9, 10, 18, 19, 20),
      CSPOMDomain.valueOf("1 3..10 18..20"))
    assertEquals(new IntInterval(3, 10), CSPOMDomain.valueOf("3..10"))
    assertEquals(new IntInterval(3, 10), CSPOMDomain.valueOf("3..5 6 7..10"))
  }
}