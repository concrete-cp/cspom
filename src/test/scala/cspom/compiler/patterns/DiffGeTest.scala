package cspom.compiler.patterns

import cspom.constraint.FunctionalConstraint
import cspom.constraint.{ CSPOMConstraint, GeneralConstraint }
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import CSPOM.{ _ }
import java.util.LinkedList
import org.junit.Assert._
import org.junit.Test

class DiffGeTest {
  @Test
  def testGen() {
    var sub: FunctionalConstraint = null
    val cspom = CSPOM {
      cspom: CSPOM =>

        val r = aux()(cspom)
        assertTrue(r.auxiliary)
        sub = new FunctionalConstraint(r, "sub", cspom.varOf(1, 2, 3), cspom.varOf(2, 3, 4))
        cspom.addConstraint(sub)

        ctr(r.>=(cspom.interVar(0, 5))(cspom))
    }
    new DiffGe(cspom).compile(sub)
    //println(cspom)
    assertEquals(3, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    assertEquals("diffGe", cspom.constraints.iterator.next.description)
    assertTrue(cspom.constraints.iterator.next.isInstanceOf[GeneralConstraint])
  }

  @Test
  def testFunc() {
    var sub: FunctionalConstraint = null
    val cspom = CSPOM {
      cspom: CSPOM =>

        val r = aux()(cspom)
        sub = new FunctionalConstraint(r, "sub", cspom.varOf(1, 2, 3), cspom.varOf(2, 3, 4))
        cspom.addConstraint(sub)

        ctr(cspom.boolVar().==(r.>=(cspom.interVar(0, 5))(cspom))(cspom))
    }
    new DiffGe(cspom).compile(sub)
    //println(cspom)
    assertEquals(4, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    assertEquals("diffGe", cspom.constraints.iterator.next.description)
    assertTrue(cspom.constraints.iterator.next.isInstanceOf[FunctionalConstraint])
  }

}