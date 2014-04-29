package cspom.compiler

import cspom.CSPOM
import CSPOM._
import cspom.variable.CSPOMConstant
import cspom.variable.IntVariable
import cspom.CSPOMConstraint
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.OptionValues

class MergeEqTest extends FlatSpec with Matchers with OptionValues {

  "MergeEq" should "simplify Variable = Constant" in {

    val problem = CSPOM { implicit problem =>
      val v1 = IntVariable(0 to 10) as "V1"
      val v2 = CSPOMConstant(0)

      ctr(v1 === v2)
    }

    ProblemCompiler.compile(problem, Seq(MergeSame, MergeEq))

    //println(problem.namedExpressions("V1").params)

    problem.namedExpressions.toSeq match {
      case Seq(("V1", CSPOMConstant(0))) =>
      case _ => fail()
    }

  }

  it should "simplify two Variables" in {

    val cspom = CSPOM { implicit problem =>
      val v0 = IntVariable(Seq(1, 2, 3)) as "V0"
      ctr(CSPOMConstraint('dummy, Seq(v0)))
      val v1 = IntVariable(Seq(2, 3, 4))
      ctr(v0 === v1)
    }

    ProblemCompiler.compile(cspom, Seq(MergeSame, MergeEq))

    val nv0 = cspom.expression("V0") collect {
      case v: IntVariable => v
    }

    cspom.namedExpressions should have size 1
    nv0.value should be theSameInstanceAs cspom.namedExpressions.head._2
    cspom.constraints should have size 1
    nv0.value.domain shouldBe Set(2, 3)

  }

}