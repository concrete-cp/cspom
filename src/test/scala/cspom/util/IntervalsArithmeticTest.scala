package cspom.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import IntervalsArithmetic.Arithmetics
import IntervalsArithmetic.RangeArithmetics
import org.scalatest.concurrent.TimeLimits

class IntervalsArithmeticTest extends FlatSpec with Matchers with PropertyChecks with TimeLimits {
  "Intervals" should "provide correct shrinking integer division" in {
    assert((IntInterval(1, 19) / 20).isEmpty)
    IntInterval(0, 19) / 20 shouldBe IntInterval(0, 0)
    IntInterval(1, 20) / 20 shouldBe IntInterval(1, 1)
    IntInterval(0, 20) / 20 shouldBe IntInterval(0, 1)

    IntInterval(1, 19) / -20 shouldBe empty
    IntInterval(0, 19) / -20 shouldBe IntInterval(0, 0)
    IntInterval(1, 20) / -20 shouldBe IntInterval(-1, -1)
    IntInterval(0, 20) / -20 shouldBe IntInterval(-1, 0)

    IntInterval.atLeast(2) / 20 shouldBe IntInterval.atLeast(1)
  }

  it should "multiply" in {
    IntInterval(0, 5) * IntInterval.singleton(10) shouldBe IntInterval(0, 50)
    (RangeSet(IntInterval(0, 100)) -- IntInterval(10, 90)) * RangeSet(IntInterval.singleton(10)) shouldBe
      RangeSet(Seq(IntInterval(0, 90), IntInterval(910, 1000)))

    IntInterval.atLeast(10) * IntInterval.singleton(100) shouldBe IntInterval.atLeast(1000)
  }

  it should "compute abs" in {
    IntInterval(5, 10).abs shouldBe IntInterval(5, 10)
    IntInterval(-10, -5).abs shouldBe IntInterval(5, 10)
    IntInterval(-10, 0).abs shouldBe IntInterval(0, 10)
    IntInterval(0, 10).abs shouldBe IntInterval(0, 10)
    IntInterval(-5, 10).abs shouldBe IntInterval(0, 10)
  }

  it should "add" in {

    ((RangeSet(IntInterval(0, 100)) -- IntInterval(10, 90)) + RangeSet(IntInterval(0, 10))) shouldBe
      RangeSet(Seq(IntInterval(0, 19), IntInterval(91, 110)))

  }

  it should "handle infinities" in {
    IntInterval.all * IntInterval.singleton(1) shouldBe IntInterval.all
    IntInterval(1, 1) + IntInterval.atLeast(0) shouldBe IntInterval.atLeast(1)
    IntInterval(1, 1) * IntInterval.atLeast(0) shouldBe IntInterval.atLeast(0)
    IntInterval(1, 1) / IntInterval.atLeast(1) shouldBe IntInterval(0, 1)
    IntInterval.atLeast(10) / 2 shouldBe IntInterval.atLeast(5)
    IntInterval.all / 2 shouldBe IntInterval.all

    IntInterval.all + IntInterval.singleton(0) shouldBe IntInterval.all

  }

}