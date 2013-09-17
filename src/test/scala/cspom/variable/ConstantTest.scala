package cspom.variable;

import org.junit.Assert.assertEquals;
import org.junit.Assert.assertFalse;

import scala.util.Random;

import org.junit.Before;
import org.junit.Test;

class ConstantTest() {

  val TEST_CONSTANT_INT = Random.nextInt;
  val TEST_CONSTANT_DOUBLE = Random.nextDouble;
  val intConstant = IntConstant(TEST_CONSTANT_INT);
  val doubleConstant = DoubleConstant(TEST_CONSTANT_DOUBLE);

  @Test
  def testEquals = {
    assertEquals(intConstant, IntConstant(TEST_CONSTANT_INT));
    assertEquals(doubleConstant, DoubleConstant(TEST_CONSTANT_DOUBLE));
  }

  @Test
  def testHashCode = {
    assertEquals(intConstant.hashCode, IntConstant(TEST_CONSTANT_INT).hashCode);
    assertEquals(doubleConstant.hashCode, DoubleConstant(TEST_CONSTANT_DOUBLE).hashCode);
  }

  @Test
  def testGetValue() {
    assertEquals(intConstant.value, TEST_CONSTANT_INT);
    assertEquals(doubleConstant.value, TEST_CONSTANT_DOUBLE, 0.0);
  }
}
