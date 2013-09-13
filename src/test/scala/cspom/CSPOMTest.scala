package cspom

import cspom.constraint.GeneralConstraint
import org.junit.Assert._
import org.junit.Test
import variable.CSPOMVariable

class CSPOMTest {

  @Test
  def variables {
    val cspom = new CSPOM
    val vars = List(
      CSPOMVariable.ofInterval("test1", 0, 10),
      CSPOMVariable.ofInterval("test2", 10, 20),
      CSPOMVariable.ofInterval("test3", 20, 30),
      CSPOMVariable.ofInterval("test4", 30, 40))

    vars foreach { cspom.addVariable(_) }

    assertTrue(vars sameElements cspom.variables)
    assertEquals(Some(vars(0)), cspom.variable("test1"))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def duplicateVariable {
    val cspom = new CSPOM
    cspom.addVariable(CSPOMVariable.ofInterval("Test", 0, 10))
    cspom.addVariable(CSPOMVariable.ofInterval("Test", 0, 10))
  }

  @Test
  def boolVariables {
    val cspom = new CSPOM
    cspom.addVariable(CSPOMVariable.bool())
    cspom.addVariable(CSPOMVariable.bool())
    assertEquals(2, cspom.variables.size)
  }

  @Test
  def constraints {
    val cspom = new CSPOM
    val v = List(CSPOMVariable.ofInterval("test1", 0, 10),
      CSPOMVariable.ofInterval("test2", 0, 10));

    v foreach { cspom.addVariable(_) }

    val leq = new GeneralConstraint("leq", v(0), v(1))
    cspom.addConstraint(leq);

    assertTrue(cspom.constraints(v(0)) contains leq)
    assertTrue(cspom.constraints(v(1)) contains leq)

    cspom.removeConstraint(leq)

    assertTrue(cspom.constraints.isEmpty)
    assertFalse(cspom.constraints(v(0)) contains leq)
    assertFalse(cspom.constraints(v(1)) contains leq)

    v foreach { cspom removeVariable _ }
  }

  @Test(expected = classOf[IllegalArgumentException])
  def protectedVariable {
    val cspom = new CSPOM
    val v = List(
      CSPOMVariable.ofInterval("test1", 0, 10),
      CSPOMVariable.ofInterval("test2", 0, 10));

    v.foreach(cspom.addVariable)

    val leq = new GeneralConstraint("leq", v(0), v(1))
    cspom.addConstraint(leq);
    cspom.removeVariable(v(1))
  }
}