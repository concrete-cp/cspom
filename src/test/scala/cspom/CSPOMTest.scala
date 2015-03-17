package cspom

import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import CSPOM._
import cspom.variable.BoolVariable
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.OptionValues
import cspom.variable.CSPOMSeq

class CSPOMTest extends FlatSpec with Matchers with OptionValues {

  "CSPOM" should "accept variables" in {
    var vars: List[CSPOMVariable[Int]] = null

    val cspom = CSPOM { implicit problem =>
      vars = List(
        IntVariable(0 to 10) as "test1",
        IntVariable(10 to 20) as "test2",
        IntVariable(20 to 30) as "test3",
        IntVariable(30 to 40) as "test4")

    }

    vars should contain theSameElementsAs cspom.namedExpressions.values

    cspom.expression("test1").value shouldBe vars(0)
  }

  it should "throw exception in case of same names" in {
    an[IllegalArgumentException] should be thrownBy
      CSPOM { implicit problem =>
        IntVariable(0 to 10) as "Test"
        IntVariable(0 to 10) as "Test"
      }
  }

  it should "accept and reference bool variables" in {
    val cspom = CSPOM { implicit problem =>
      ctr(CSPOMConstraint('dummy, Seq(new BoolVariable())))
      ctr(CSPOMConstraint('dummy, Seq(new BoolVariable())))
    }
    // Third is CSPOMConstant(true) !
    cspom.referencedExpressions should have size 3
  }

  it should "reference and dereference constraints" in {
    var leq: CSPOMConstraint[Boolean] = null
    var v: List[CSPOMVariable[Int]] = null

    val cspom = CSPOM { implicit problem =>
      v = List(
        IntVariable(0 to 10),
        IntVariable(0 to 10));

      //v foreach { cspom.addVariable(_) }

      leq = ctr(CSPOMConstraint('leq, Seq(v(0), v(1))))
    }

    cspom.constraints(v(0)) should contain(leq)
    cspom.constraints(v(1)) should contain(leq)

    cspom.removeConstraint(leq)

    cspom.constraints should not be ('hasNext)

    cspom.constraints(v(0)) should not contain (leq)
    cspom.constraints(v(1)) should not contain (leq)

    cspom.referencedExpressions shouldBe empty

  }

  it should "generate proper GML" in {
    val cspom = CSPOM { implicit problem =>
      val (x, y, z) = (
        IntVariable(0 to 10) as "X",
        IntVariable(0 to 10) as "Y",
        IntVariable(0 to 10) as "Z");

      ctr(CSPOMConstraint('leq, Seq(x, y)))
      ctr(CSPOMConstraint('leq, Seq(x, y, z)))

    }

    val gml = cspom.toGML

    //shouldBe """graph [
    //directed 0
    //
    //          node [
    //            id "X"
    //            label "X"
    //          ]
    //          
    //          node [
    //            id "Y"
    //            label "Y"
    //          ]
    //          
    //          node [
    //            id "Z"
    //            label "Z"
    //          ]
    //          
    //          edge [
    //            source "X"
    //            target "Y"
    //            label "leq"
    //          ]
    //          
    //          node [
    //            id "cons1"
    //            label "leq"
    //            graphics [ fill "#FFAA00" ]
    //          ]
    //          
    //          edge [
    //            source "cons1"
    //            target "X"
    //          ]
    //          
    //          edge [
    //            source "cons1"
    //            target "Y"
    //          ]
    //          
    //          edge [
    //            source "cons1"
    //            target "Z"
    //          ]
    //          ]
    //"""
  }

  it should "correctly replace variables" in {
    val cspom = CSPOM { implicit problem =>
      val x = IntVariable(0 to 10) as "X"
      val y = IntVariable(10 to 20) as "Y"
      Seq(x, y) as "T"
    }

    val x = cspom.expression("X").get

    val y = cspom.expression("Y").get
    val rx = IntVariable(20 to 30)

    cspom.replaceExpression(x, rx)
    val t = cspom.expression("T").get

    t shouldBe CSPOMSeq(rx, y)
    cspom.expression("X").value shouldBe rx
    cspom.expression("T[0]").value shouldBe rx
    cspom.expression("T[1]").value shouldBe y
    cspom.referencedExpressions should contain theSameElementsAs Seq(rx, y, t)

    cspom.replaceExpression(t, x)
    cspom.expression("X").value shouldBe rx
    cspom.expression("T").value shouldBe x
    cspom.expression("T[0]") shouldBe None
    cspom.referencedExpressions should contain theSameElementsAs Seq(rx, y, x)

  }
}