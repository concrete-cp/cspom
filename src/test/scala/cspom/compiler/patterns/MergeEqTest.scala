package cspom.compiler.patterns

import cspom.constraint.{CSPOMConstraint, GeneralConstraint}
import cspom.variable.{ExtensiveDomain, CSPOMVariable}
import cspom.CSPOM
import org.junit.Assert._
import org.junit.Test
import scala.collection.mutable.Queue

class MergeEqTest {
  @Test
  def testExt() {
    val cspom = new CSPOM
    val v0 = cspom.varOf(1, 2, 3)
    val v1 = cspom.addVariable(CSPOMVariable.aux())
    v1.domain = ExtensiveDomain.of(2, 3, 4)
    assertTrue(v1.auxiliary)
    val eq = new GeneralConstraint("eq", v0, v1)
    cspom.addConstraint(eq)

    new MergeEq(cspom, new Queue[CSPOMConstraint]).compile(eq);

    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain.values)

  }

  @Test
  def testInt() {
    val cspom = new CSPOM
    val v0 = cspom.interVar(1, 3)
    val v1 = cspom.addVariable(CSPOMVariable.aux())
    v1.domain = ExtensiveDomain.of(2, 3, 4)
    assertTrue(v1.auxiliary)
    val eq = new GeneralConstraint("eq", v0, v1)
    cspom.addConstraint(eq)

    new MergeEq(cspom, new Queue[CSPOMConstraint]).compile(eq);

    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain.values)

  }
}