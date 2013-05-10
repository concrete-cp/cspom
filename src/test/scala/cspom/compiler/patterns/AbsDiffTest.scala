package cspom.compiler.patterns

import cspom.constraint.FunctionalConstraint
import cspom.constraint.{ CSPOMConstraint, GeneralConstraint }
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import CSPOM.{ _ }
import java.util.LinkedList
import org.junit.Assert._
import org.junit.Test

class AbsDiffTest {
  @Test
  def testExt() {
    val (cspom, sub) = CSPOM withResult {

      val v0 = varOf(1, 2, 3)
      val v1 = varOf(2, 3, 4)
      val r = aux()
      assertTrue(r.auxiliary)

      val r2 = 'abs(r)

      threadLocalProblem.addConstraint(new FunctionalConstraint(r, "sub", v0, v1))
    }
    println(cspom)
    new AbsDiff(cspom).compile(sub)
    println(cspom)
    assertEquals(3, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    assertEquals("absdiff", cspom.constraints.head.description)
    assertTrue(cspom.constraints.head.isInstanceOf[FunctionalConstraint])
  }

}