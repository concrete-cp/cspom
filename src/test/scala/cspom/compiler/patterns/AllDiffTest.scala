package cspom.compiler.patterns

import org.junit.Assert.assertEquals
import org.junit.Test

import cspom.constraint.GeneralConstraint
import cspom.CSPOM
import CSPOM._

class AllDiffTest {
  @Test
  def testExt() {
    val cspom = CSPOM {
      val v0 = varOf(1, 2, 3)
      val v1 = varOf(2, 3, 4)
      val v2 = varOf(1, 2, 3)
      val v3 = varOf(1, 2, 3)

      for (p <- List(v0, v1, v2, v3).combinations(2)) {
        ctr("ne", p: _*)
      }
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