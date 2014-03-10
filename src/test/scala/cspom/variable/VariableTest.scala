package cspom.variable

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

import cspom.CSPOM
import cspom.CSPOMConstraint

class VariableTest {
  @Test
  def registerTest() {
    val v = IntVariable(0 to 10)
    val c = CSPOMConstraint('leq, v)
    val cspom = new CSPOM
    //cspom.addVariable(v)
    cspom.ctr(c)
    //v registerConstraint c
    assertEquals(cspom.referencedExpressions, Set(CSPOMTrue, v))
    assertTrue(cspom.constraints(v) sameElements List(c))
  }

  @Test
  def containsTest() {
    val vb = new BoolVariable()

    assertTrue(vb.contains(true))
    assertFalse(vb.contains(0))
    assertTrue(vb.contains(false))

    val vi = IntVariable(0 to 10)
    
    assertTrue(vi.contains(5))
    assertFalse(vi.contains(15))
    assertFalse(vi.contains(false))
    
    val vf = IntVariable.free()
    assertTrue(vf.contains(5))
    assertFalse(vf.contains(true))
  }
}