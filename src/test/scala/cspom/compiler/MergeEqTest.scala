package cspom.compiler

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues

import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression

class MergeEqTest extends FlatSpec with Matchers with OptionValues {
  "MergeEq" should "simplify booleans" in {
    val cspom = CSPOM { implicit problem =>
      val vars = Seq.tabulate(4)(i => new BoolVariable as s"b$i")

      val contents = Array.fill[CSPOMExpression[Boolean]](4)(new BoolVariable)
      contents(2) = CSPOMConstant(false)
      val seq = contents.toSeq as "array"

      for ((v1, v2) <- vars zip seq) {
        ctr(CSPOMConstraint('eq)(v1, v2))
      }

      ctr(CSPOMConstraint('clause)(Seq(contents(1), contents(2)), Seq(contents(3))))
      ctr(CSPOMConstraint('clause)(Seq(vars(0)), Seq(vars(1))))

    }

    //println("Before merging")

    //println(cspom)

    CSPOMCompiler.compile(cspom, Seq(MergeEq))
    //println("After merging")
    //println(cspom)

    cspom.referencedExpressions should have size 6

    val array = cspom.expression(s"array").value.asInstanceOf[CSPOMSeq[_]]

    for (i <- 0 until 4) {
      val b = cspom.expression(s"b$i").value
      cspom.expression(s"array[$i]").value shouldBe theSameInstanceAs(b)
      cspom.getContainers(b) should contain((array, i))
      array(i) shouldBe theSameInstanceAs(b)
      cspom.namesOf(b) should contain theSameElementsAs Seq(s"b$i", s"array[$i]")
    }

  }

  it should "simplify Variable = Constant" in {

    val problem = CSPOM { implicit problem =>
      val v1 = IntVariable(0 to 10) as "V1"
      val v2 = CSPOMConstant(0)

      ctr(v1 === v2)
    }

    CSPOMCompiler.compile(problem, Seq(MergeSame, MergeEq))

    //println(problem.namedExpressions("V1").params)

    problem.expressionsWithNames match {
      case Seq(("V1", CSPOMConstant(0))) =>
      case _                             => fail()
    }

  }

  it should "simplify two Variables" in {

    val cspom = CSPOM { implicit problem =>
      val v0 = IntVariable(1, 2, 3) as "V0"
      ctr(CSPOMConstraint('dummy)(v0))
      val v1 = IntVariable(2, 3, 4)
      ctr(v0 === v1)
    }

    CSPOMCompiler.compile(cspom, Seq(MergeSame, MergeEq))

    val nv0 = cspom.expression("V0") collect {
      case v: SimpleExpression[_] => v
    }

    withClue(cspom) {

      cspom.expressionsWithNames should have size 1
      nv0.value should be theSameInstanceAs cspom.expressionsWithNames.head._2
      cspom.constraints should have size 1
      nv0.value.asInstanceOf[IntVariable].asSortedSet should contain theSameElementsAs Set(2, 3)

    }
  }

}