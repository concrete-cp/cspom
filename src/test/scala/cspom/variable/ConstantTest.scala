package cspom.variable

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConstantTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

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
