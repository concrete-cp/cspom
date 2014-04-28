package cspom

import org.junit.Assert._
import org.junit.Test
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import CSPOM._
import cspom.variable.BoolVariable
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.OptionValues

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
      val v = List(
        IntVariable(0 to 10),
        IntVariable(0 to 10),
        IntVariable(0 to 10));

      ctr(CSPOMConstraint('leq, v.take(2)))
      ctr(CSPOMConstraint('leq, v))

    }

    cspom.toGML shouldBe """graph [
directed 0

          node [
            id "_1"
            label "_1"
          ]
          
          node [
            id "_2"
            label "_2"
          ]
          
          node [
            id "_3"
            label "_3"
          ]
          
          edge [
            source "_3"
            target "_1"
            label "leq"
          ]
          
          node [
            id "cons1"
            label "leq"
            graphics [ fill "#FFAA00" ]
          ]
          
          edge [
            source "cons1"
            target "_3"
          ]
          
          edge [
            source "cons1"
            target "_1"
          ]
          
          edge [
            source "cons1"
            target "_2"
          ]
          ]
"""
  }
}