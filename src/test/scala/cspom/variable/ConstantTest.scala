package cspom.variable;

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import scala.util.Random
import org.junit.Before
import org.junit.Test;
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import org.scalatest.FlatSpec

class ConstantTest extends FlatSpec with Matchers with PropertyChecks {

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
