package cspom.compiler.patterns

import cspom.constraint.{CSPOMConstraint, GeneralConstraint}
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import java.util.LinkedList
import org.junit.Assert._
import org.junit.Test

class MergeEqTest {
  @Test
  def testExt() {
    val cspom = new CSPOM
    val v0 = cspom.varOf(1, 2, 3)
    val v1 = cspom.addVariable(CSPOMVariable.ofSeq("aux", true, List(2, 3, 4)))
    assertTrue(v1.auxiliary)
    val eq = new GeneralConstraint("eq", v0, v1)
    cspom.addConstraint(eq)
    
    new MergeEq(cspom, new LinkedList[CSPOMConstraint]()).compile(eq);
    
    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain.values)
   
  }
  
   @Test
  def testInt() {
    val cspom = new CSPOM
    val v0 = cspom.interVar(1, 3)
    val v1 = cspom.addVariable(CSPOMVariable.ofSeq("aux", true, List(2, 3, 4)))
    assertTrue(v1.auxiliary)
    val eq = new GeneralConstraint("eq", v0, v1)
    cspom.addConstraint(eq)
    
    new MergeEq(cspom, new LinkedList[CSPOMConstraint]()).compile(eq);
    
    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain.values)
   
  }
}