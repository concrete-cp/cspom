package cspom.compiler.patterns

import cspom.constraint.FunctionalConstraint
import cspom.constraint.{ CSPOMConstraint, GeneralConstraint }
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import CSPOM._
import java.util.LinkedList
import org.junit.Assert._
import org.junit.Test

class AbsDiffTest {
  @Test
  def testExt() {
    var sub: FunctionalConstraint = null
    val cspom = CSPOM {
      cspom: CSPOM =>
        val v0 = varOf(1, 2, 3)
        val v1 = varOf(2, 3, 4)
        val r = aux()
        assertTrue(r.auxiliary)

        sub = new FunctionalConstraint(r, "sub", v0, v1)
        cspom.addConstraint(sub)

        interVar(0, 5) is ("abs", r)

    }
    new AbsDiff(cspom).compile(sub)
    //println(cspom)
    assertEquals(3, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    assertEquals("absdiff", cspom.constraints.head.description)
    assertTrue(cspom.constraints.head.isInstanceOf[FunctionalConstraint])
  }

}