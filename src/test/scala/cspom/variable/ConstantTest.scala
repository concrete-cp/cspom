package cspom.variable;

import org.junit.Assert.assertEquals;
import org.junit.Assert.assertFalse;

import scala.util.Random;

import org.junit.Before;
import org.junit.Test;

class ConstantTest() {

  val TEST_CONSTANT_INT = Random.nextInt;
  val TEST_CONSTANT_DOUBLE = Random.nextDouble;
  val intConstant: Constant[Int] = new Constant(TEST_CONSTANT_INT);
  val doubleConstant: Constant[Double] = new Constant(TEST_CONSTANT_DOUBLE);

  @Test
  def testEquals = {
    assertEquals(intConstant, new Constant(TEST_CONSTANT_INT));
    assertEquals(doubleConstant, new Constant(TEST_CONSTANT_DOUBLE));
  }

  @Test
  def testHashCode = {
    assertEquals(intConstant.hashCode, new Constant(TEST_CONSTANT_INT).hashCode);
    assertEquals(doubleConstant.hashCode, new Constant(TEST_CONSTANT_DOUBLE).hashCode);
  }

  @Test
  def testGetValue() {
    assertEquals(intConstant.value, TEST_CONSTANT_INT);
    assertEquals(doubleConstant.value, TEST_CONSTANT_DOUBLE, 0.0);
  }
}
