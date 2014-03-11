package cspom.compiler

import org.junit.Test
import cspom.CSPOM
import CSPOM._
import cspom.variable.CSPOMConstant
import cspom.variable.IntVariable
import cspom.CSPOMConstraint
import org.junit.Assert._
import junit.framework.AssertionFailedError

class MergeEqTest {

  @Test
  def test1() {
    val problem = CSPOM {
      val v1 = IntVariable(0 to 10, Map("output_var" -> Unit)) as "V1"
      val v2 = CSPOMConstant(0)

      ctr(CSPOMConstraint('eq, v1, v2))
    }

    ProblemCompiler.compile(problem, StandardCompilers.improve() ++ StandardCompilers())

    println(problem.namedExpressions("V1").params)

    assertEquals(Map("V1" -> CSPOMConstant(0, Map("output_var" -> Unit))), problem.namedExpressions)

  }

  @Test
  def testExt() {

    val cspom = CSPOM {
      val v0 = IntVariable(Seq(1, 2, 3)) as "V0"
      ctr(CSPOMConstraint('dummy, v0))
      val v1 = IntVariable(Seq(2, 3, 4), Map("var_is_introduced" -> Unit))
      ctr(v0 === v1)
    }

    ProblemCompiler.compile(cspom, Seq(MergeSame, MergeEq))

    val nv0 = cspom.expression("V0") collect {
      case v: IntVariable => v
    } getOrElse {
      throw new AssertionFailedError()
    }
    assertEquals(1, cspom.namedExpressions.size)
    assertSame(nv0, cspom.namedExpressions.head._2)
    assertEquals(cspom.toString, 1, cspom.constraints.size)
    assertEquals(List(2, 3), nv0.domain)

  }

}