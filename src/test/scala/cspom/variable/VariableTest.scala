package cspom.variable

import org.junit.Assert._
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
}