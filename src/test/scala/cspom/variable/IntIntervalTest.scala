package cspom.variable;

import org.junit.Test
import org.junit.Before
import org.junit.Assert._

final class IntIntervalTest {

  var intInterval: IntInterval = null;

  @Before
  def setUp() {
    intInterval = new IntInterval(10, 15);
  }

  @Test
  def testInterval() {
    assertEquals(10, intInterval.lb);
    assertEquals(15, intInterval.ub);
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testBadInterval() {
    new IntInterval(15, 10);
  }

  @Test
  def testEquals() {
    assertEquals(intInterval, new IntInterval(10, 15));
  }

  @Test
  def testToString() {
    assertEquals("[10..15]", intInterval.toString);
  }

  @Test
  def testGetValues() {
    assertEquals(List(10, 11, 12, 13, 14, 15), intInterval);
  }

  @Test
  def testValueOf() {
    assertEquals(intInterval, IntInterval.valueOf("10..15"));
  }
}
