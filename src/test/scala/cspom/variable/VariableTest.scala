package cspom.variable

import cspom.CSPOM._
import cspom.util.{IntInterval, Intervals, RangeSet}
import cspom.{CSPOM, CSPOMConstraint}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class VariableTest extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

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

    forAll { i: Boolean => assert(vb.contains(i)) }

    forAll { i: Int => vb.contains(i) shouldBe (i == 0 || i == 1) }

    forAll { i: Double => assert(!vb.contains(i)) }
  }

  "Int variables" should "work as sets" in {

    val v = IntVariable(RangeSet(IntInterval(-2147483648, 2088690084)))

    forAll { i: Int => v.contains(i) shouldBe (i >= -2147483648 && i <= 2088690084) }

    forAll(Intervals.validIntervals) { itv =>
      val vi = IntVariable(itv)

      forAll { i: Int =>
        vi.contains(i) shouldBe itv.contains(i)
      }

      forAll { i: Boolean =>
        if (i) {
          vi.contains(i) shouldBe itv.contains(1)
        } else {
          vi.contains(i) shouldBe itv.contains(0)
        }
      }

      forAll { i: Double => assert(!vi.contains(i)) }
    }

  }

  "Integer free variables" should "work as sets" in {
    val vf = IntVariable.free()

    forAll { i: Boolean => assert(vf.contains(i)) }

    forAll { i: Int => assert(vf.contains(i)) }

    forAll { i: Double => assert(!vf.contains(i)) }
  }
}