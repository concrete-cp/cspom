package cspom.compiler.patterns

import cspom.constraint.FunctionalConstraint
import cspom.constraint.{ CSPOMConstraint, GeneralConstraint }
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import java.util.LinkedList
import org.junit.Assert._
import org.junit.Test

class AllDiffTest {
  @Test
  def testExt() {
    val cspom = new CSPOM
    val v0 = cspom.varOf(1, 2, 3)
    val v1 = cspom.varOf(2, 3, 4)
    val v2 = cspom.varOf(1, 2, 3)
    val v3 = cspom.varOf(1, 2, 3)
    for (p <- List(v0, v1, v2, v3).combinations(2)) {
      cspom.addConstraint(new GeneralConstraint(
        description = "ne",
        scope = p))
    }

    val allDiff = new AllDiff(cspom);
    val constraints = cspom.constraints.toSeq

    for (c <- constraints if (cspom.constraints.contains(c))) {
        allDiff.compile(c)
    }

    //println(cspom)
    assertEquals(1, cspom.constraints.size)
    assertEquals("allDifferent", cspom.constraints.head.description)
  }

}