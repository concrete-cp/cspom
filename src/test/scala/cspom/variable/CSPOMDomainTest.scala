package cspom.variable

import org.junit.Test
import org.junit.Assert._
class CSPOMDomainTest {
  @Test
  def test() {
    val domain = CSPOMDomain.valueOf("1 3..10 18..20")
    assertEquals(Seq(1, 3, 4, 5, 6, 7, 8, 9, 10, 18, 19, 20), domain.values)
  }
}