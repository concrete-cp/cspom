package cspom.variable

import cspom.constraint.GeneralConstraint
import org.junit.Assert._
import org.junit.Test

class VariableTest {
  @Test
  def registerTest() {
    val v = CSPOMVariable.ofInterval(lb = 0, ub = 10)
    val c = new GeneralConstraint("leq", v)
    v registerConstraint c
    assertTrue(v.constraints sameElements List(c))
  }
}