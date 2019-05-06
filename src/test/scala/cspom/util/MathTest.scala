package cspom.util

import cspom.util.Math.Rational
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class MathTest extends FlatSpec with Matchers with PropertyChecks {
  "GCD" should "compute properly" in {
    forAll { (a: Int, b: Int) =>
      whenever(a > Int.MinValue && b > Int.MinValue) {
        val gcd = Math.gcd(a, b)
        whenever(a != 0 && b != 0) {
          withClue(s"gcd = $gcd") {
            //println(a,b)
            val remA = a / gcd
            remA * gcd shouldBe a
            val remB = b / gcd
            remB * gcd shouldBe b

            Math.gcd(remA, remB) shouldBe 1


            a.toDouble / b shouldBe remA.toDouble / remB
            b.toDouble / a shouldBe remB.toDouble / remA
          }
        }
      }
    }
  }
  "Rationals" should "add and simplify" in {
    Rational(1, 2) + Rational(-2, 3) shouldBe Rational(-1, 6)
  }

  it should "add and simplify property" in {
    forAll(minSuccessful(10000)) { (a: Int, b: Int, c: Int, d: Int) =>
      whenever(b != 0 && d != 0) {
        val f = Rational(a, b)
        val g = Rational(c, d)
        val sum = f + g
        val floating = a.toDouble / b + c.toDouble / d
        withClue((f, g, sum)) {
          if (floating == 0) {
            sum shouldBe Rational.zero
          } else {
            sum.toDouble / floating === (1.0 +- 1e-15)
          }
        }
      }
    }
  }

  it should "sub and simplify property" in {
    forAll(minSuccessful(10000)) { (a: Int, b: Int, c: Int, d: Int) =>
      whenever(b != 0 && d != 0) {
        val f = Rational(a, b)
        val g = Rational(c, d)
        val diff = f - g
        val floating = a.toDouble / b - c.toDouble / d
        withClue((f, g, diff)) {
          if (floating == 0) {
            diff shouldBe Rational.zero
          } else {
            diff.toDouble / floating === (1.0 +- 1e-15)
          }
        }
      }
    }
  }

  it should "have valid hashcodes" in {
    forAll(maxDiscardedFactor(1000.0)) { (a: Int, b: Int, c: Int, d: Int) =>
      whenever(b != 0 && d != 0) {
        whenever(Rational(a, b) == Rational(c, d)) {
          Rational(a, b).hashCode shouldBe Rational(c, d).hashCode
        }
      }
    }
  }

  private def myDoubleCompare(a: Double, b: Double) = {
    // Allows -0.0 == 0.0
    if (a == b) 0 else a.compare(b)
  }

  it should "compare correctly" in {
    forAll(minSuccessful(10000)) { (a: Int, b: Int, c: Int, d: Int) =>
      whenever(b != 0 && d != 0) {

        Rational(a, b).compare(Rational(c, d)) shouldBe myDoubleCompare(a.toDouble / b, c.toDouble / d)
      }
    }
  }
}