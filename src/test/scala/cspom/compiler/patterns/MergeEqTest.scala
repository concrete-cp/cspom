package cspom.compiler.patterns

import cspom.constraint.{ CSPOMConstraint, GeneralConstraint }
import cspom.variable.{ ExtensiveDomain, CSPOMVariable }
import cspom.CSPOM
import CSPOM._
import org.junit.Assert._
import org.junit.Test
import scala.collection.mutable.Queue
import cspom.variable.AuxVar

class MergeEqTest {
  @Test
  def testExt() {
    var eq: CSPOMConstraint = null
    var v0: CSPOMVariable = null
    val cspom = CSPOM {
      v0 = varOf(1, 2, 3)
      val v1 = aux()
      v1.domain = ExtensiveDomain.of(2, 3, 4)
      assertTrue(v1.auxiliary)
      eq = ctr(v0 == v1) //ctr("eq", v0, v1)
    }

    new MergeEq(cspom, new Queue[CSPOMConstraint]).compile(eq);

    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain.values)

  }

  @Test
  def testInt() {
    var eq: CSPOMConstraint = null
    var v0: CSPOMVariable = null

    val cspom = CSPOM { cspom: CSPOM =>
      v0 = cspom.interVar(1, 3)
      val v1 = cspom.addVariable(new AuxVar())
      v1.domain = ExtensiveDomain.of(2, 3, 4)
      assertTrue(v1.auxiliary)
      eq = ctr(v0.==(v1)(cspom))
    }
    new MergeEq(cspom, new Queue[CSPOMConstraint]).compile(eq);

    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain.values)

  }
}