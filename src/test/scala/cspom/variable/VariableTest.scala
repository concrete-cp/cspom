package cspom.variable

import cspom.CSPOM
import cspom.CSPOMConstraint
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import CSPOM._
import org.scalatest.prop.PropertyChecks
import cspom.util.Intervals
import cspom.util.IntInterval
import cspom.util.RangeSet

class VariableTest extends FlatSpec with Matchers with PropertyChecks {

  "CSPOM" should "register variables" in {
    var v: IntVariable = null
    var c: CSPOMConstraint[_] = null
    val cspom = CSPOM { implicit problem =>
      v = IntVariable(0 to 10)
      c = ctr(CSPOMConstraint('leq)(v))
    }
    cspom.referencedExpressions should contain theSameElementsAs Seq(CSPOMConstant(true), v)
    cspom.constraints(v) should contain theSameElementsAs Seq(c)

  }

  "Boolean variables" should "work as sets" in {
    val vb = new BoolVariable()

    forAll { i: Boolean => vb.contains(i) shouldBe true }

    forAll { i: Int => vb.contains(i) shouldBe false }

    forAll { i: Double => vb.contains(i) shouldBe false }
  }

  "Int variables" should "work as sets" in {

    val v = IntVariable(RangeSet(IntInterval(-2147483648, 2088690084)))

    forAll { i: Int => v.contains(i) shouldBe (i >= -2147483648 && i <= 2088690084) }

    forAll(Intervals.validIntervals) { itv =>
      val vi = IntVariable(itv)

      forAll { i: Int =>
        vi.contains(i) shouldBe itv.contains(i)
      }

      forAll { i: Boolean => vi.contains(i) shouldBe false }

      forAll { i: Double => vi.contains(i) shouldBe false }
    }

  }

  "Integer free variables" should "work as sets" in {
    val vf = IntVariable.free()

    forAll { i: Boolean => vf.contains(i) shouldBe false }

    forAll { i: Int => vf.contains(i) shouldBe true }

    forAll { i: Double => vf.contains(i) shouldBe false }
  }
}