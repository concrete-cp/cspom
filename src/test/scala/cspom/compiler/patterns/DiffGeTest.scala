package cspom.compiler.patterns

import cspom.constraint.FunctionalConstraint
import cspom.constraint.{ CSPOMConstraint, GeneralConstraint }
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import java.util.LinkedList
import org.junit.Assert._
import org.junit.Test

class DiffGeTest {
  @Test
  def testGen() {
    val cspom = new CSPOM
    val r = cspom.addVariable(CSPOMVariable.aux())
    assertTrue(r.auxiliary)
    val sub = new FunctionalConstraint(r, "sub", cspom.varOf(1, 2, 3), cspom.varOf(2, 3, 4))
    cspom.addConstraint(sub)

    cspom.addConstraint(new GeneralConstraint("ge", r, cspom.interVar(0, 5)))

    new DiffGe(cspom).compile(sub)
    //println(cspom)
    assertEquals(3, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    assertEquals("diffGe", cspom.constraints.iterator.next.description)
    assertTrue(cspom.constraints.iterator.next.isInstanceOf[GeneralConstraint])
  }

  @Test
  def testFunc() {
    val cspom = new CSPOM
    val r = cspom.addVariable(CSPOMVariable.aux())
    val sub = new FunctionalConstraint(r, "sub", cspom.varOf(1, 2, 3), cspom.varOf(2, 3, 4))
    cspom.addConstraint(sub)

    cspom.addConstraint(new FunctionalConstraint(cspom.boolVar, "ge", r, cspom.interVar(0, 5)))

    new DiffGe(cspom).compile(sub)
    //println(cspom)
    assertEquals(4, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    assertEquals("diffGe", cspom.constraints.iterator.next.description)
    assertTrue(cspom.constraints.iterator.next.isInstanceOf[FunctionalConstraint])
  }

}