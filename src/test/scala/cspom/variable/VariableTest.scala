package cspom.variable

import org.junit.Assert._
import org.junit.Test
import cspom.CSPOM
import cspom.CSPOMConstraint

class VariableTest {
  @Test
  def registerTest() {
    val v = CSPOMVariable.ofInterval(lb = 0, ub = 10)
    val c = new CSPOMConstraint("leq", v)
    val cspom = new CSPOM
    cspom.addVariable(v)
    cspom.addConstraint(c)
    //v registerConstraint c
    assertTrue(cspom.constraints(v) sameElements List(c))
  }
}