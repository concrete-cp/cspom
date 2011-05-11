package cspom.compiler.patterns

import cspom.constraint.FunctionalConstraint
import cspom.constraint.{CSPOMConstraint, GeneralConstraint}
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import java.util.LinkedList
import org.junit.Assert._
import org.junit.Test

class AbsDiffTest {
  @Test
  def testExt() {
    val cspom = new CSPOM
    val v0 = cspom.varOf(1, 2, 3)
    val v1 = cspom.varOf(2, 3, 4)
    val r = cspom.addVariable(CSPOMVariable.aux())
    assertTrue(r.auxiliary)
    val sub = new FunctionalConstraint(r, "sub", v0, v1)
    cspom.addConstraint(sub)
    
    val v2 = cspom.interVar(0, 5)
    cspom.addConstraint(new FunctionalConstraint(v2, "abs", r))
    
    new AbsDiff(cspom).compile(sub)
    //println(cspom)
    assertEquals(3, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    assertEquals("absdiff", cspom.constraints.iterator.next.description)
    assertTrue(cspom.constraints.iterator.next.isInstanceOf[FunctionalConstraint])
  }
  
}