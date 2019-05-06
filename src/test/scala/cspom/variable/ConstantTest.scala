package cspom.variable

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ConstantTest extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

  "Int constants" should "behave like Ints" in {

    forAll { n: Int =>
      val intConstant = CSPOMConstant(n)

      intConstant shouldBe CSPOMConstant(n)
      intConstant.hashCode shouldBe CSPOMConstant(n).hashCode()
      intConstant.value shouldBe n
    }

  }

  "Double constants" should "behave like Doubles" in {
    forAll { n: Double =>
      val doubleConstant = CSPOMConstant(n)

      doubleConstant shouldBe CSPOMConstant(n)
      doubleConstant.hashCode shouldBe CSPOMConstant(n).hashCode()
      doubleConstant.value shouldBe n
    }
  }

}
