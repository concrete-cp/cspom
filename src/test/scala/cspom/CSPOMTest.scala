package cspom

import cspom.constraint.GeneralConstraint
import cspom.constraint.FunctionalConstraint
import org.junit.Assert._
import org.junit.Test
import variable.CSPOMVariable
class CSPOMTest {

  @Test
  def variables = {
    val cspom = new CSPOM
    val vars = List(
      CSPOMVariable.ofInterval("test1", 0, 10),
      CSPOMVariable.ofInterval("test2", 10, 20),
      CSPOMVariable.ofInterval("test3", 20, 30),
      CSPOMVariable.ofInterval("test4", 30, 40))

    vars foreach { v => cspom.addVariable(v) }

    assert(vars sameElements cspom.variables)
    assertEquals(vars(0), cspom.variable("test1"))
  }

  @Test(expected = classOf[AssertionError])
  def duplicateVariable = {
    val cspom = new CSPOM
    cspom.addVariable(CSPOMVariable.ofInterval("Test", 0, 10))
    cspom.addVariable(CSPOMVariable.ofInterval("Test", 0, 10))
  }

  @Test
  def boolVariables = {
    val cspom = new CSPOM
    cspom.addVariable(CSPOMVariable.ofBool(true))
    cspom.addVariable(CSPOMVariable.ofBool(true))
    assertEquals(2, cspom.variables.size)
  }

  @Test
  def constraints = {
    val cspom = new CSPOM
    val v = List(CSPOMVariable.ofInterval("test1", 0, 10),
      CSPOMVariable.ofInterval("test2", 0, 10));

    v foreach { cspom.addVariable(_) }

    val leq = new GeneralConstraint("leq", "", v(0), v(1))
    cspom.addConstraint(leq);

    assertTrue(v(0).constraints contains leq)
    assertTrue(v(1).constraints contains leq)

    cspom.removeConstraint(leq)

    assertTrue(cspom.constraints.isEmpty)
    assertFalse(v(0).constraints contains leq)
    assertFalse(v(1).constraints contains leq)

    v foreach { cspom removeVariable _ }
  }

  @Test(expected = classOf[AssertionError])
  def protectedVariable = {
    val cspom = new CSPOM
    val v = List(CSPOMVariable.ofInterval("test1", 0, 10),
      CSPOMVariable.ofInterval("test2", 0, 10));

    v foreach { cspom.addVariable(_) }

    val leq = new GeneralConstraint("leq", "", v(0), v(1))
    cspom.addConstraint(leq);
    cspom.removeVariable(v(1))
  }
}