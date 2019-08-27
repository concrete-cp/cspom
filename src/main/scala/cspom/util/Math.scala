package cspom.util

import java.math.RoundingMode

object Math {


  def toIntExact(bi: BigInt): Int = {
    if (!bi.isValidInt) {
      throw new ArithmeticException(s"integer overflow: $bi")
    } else {
      bi.intValue
    }
  }

  def checkedPow(a: Int, b: Int): Int = {
    val r = BigInt(a).pow(b)
    if (r.isValidInt) {
      r.intValue
    } else {
      throw new ArithmeticException(s"pow($a, $b): overflow")
    }
  }

  /**
    * Returns the result of dividing {@code p} by {@code q}, rounding using the specified
    * {@code RoundingMode}.
    *
    * @throws ArithmeticException if { @code q == 0}, or if { @code mode == UNNECESSARY} and { @code a}
    *                             is not an integer multiple of { @code b}
    */
  def divide(p: Int, q: Int, mode: RoundingMode): Int = {
    import RoundingMode._

    val div = p / q
    val rem = p - q * div // equal to p % q

    if (rem == 0) {
      div
    } else {

      /*
       * Normal Java division rounds towards 0, consistently with RoundingMode.DOWN. We just have to
       * deal with the cases where rounding towards 0 is wrong, which typically depends on the sign of
       * p / q.
       *
       * signum is 1 if p and q are both nonnegative or both negative, and -1 otherwise.
       */
      val signum = 1 | ((p ^ q) >> (Integer.SIZE - 1));
      val increment = mode match {
        case UNNECESSARY if rem == 0 =>
          throw new ArithmeticException("mode was UNNECESSARY, but rounding was necessary");

        case UNNECESSARY | DOWN => false
        case UP => true
        case CEILING => signum > 0;
        case FLOOR => signum < 0;

        case HALF_EVEN | HALF_DOWN | HALF_UP =>
          val absRem = math.abs(rem)
          val cmpRemToHalfDivisor = absRem - (math.abs(q) - absRem)
          // subtracting two nonnegative ints can't overflow
          // cmpRemToHalfDivisor has the same sign as compare(abs(rem), abs(q) / 2).
          if (cmpRemToHalfDivisor == 0) { // exactly on the half mark
            mode == HALF_UP || (mode == HALF_EVEN & (div & 1) != 0)
          } else {
            cmpRemToHalfDivisor > 0; // closer to the UP value
          }
        case _ => throw new AssertionError();
      }
      if (increment) div + signum else div
    }
  }

  def divide(p: BigInt, q: BigInt, mode: RoundingMode): BigInt = {
    val pDec = BigDecimal(p).underlying
    val qDec = BigDecimal(q).underlying
    pDec.divide(qDec, 0, mode).toBigIntegerExact
  }

  def sqrt(i: BigInt): BigInt = {
    if (i == 0) 0
    else if (i > 0) {
      var root = BigInt(1) << (i.bitLength / 2)
      while (!isSqrt(i, root)) {
        root += i / root
        root /= 2
      }
      root
    } else {
      throw new IllegalArgumentException("NaN")
    }
  }

  def isSqrt(n: BigInt, r: BigInt): Boolean = {
    n >= r * r && n < (r + 1) * (r + 1)
  }

  def even(a: Int): Boolean = (a & 0x1) == 0

  def gcd(s: Seq[Long]): Long = s.reduce(gcd)

  def lcm(s: Seq[Long]): Long = s.reduce(lcm)

  def lcm(ia: Long, ib: Long): Long = {
    val gcd = this.gcd(ia, ib)
    java.lang.Math.multiplyExact(ia / gcd, ib)
  }

  def gcd(ia: Long, ib: Long): Long = {
    import java.lang.Long.numberOfTrailingZeros

    // corner cases
    if (ia == 0) return ib
    if (ib == 0) return ia

    var a = absExact(ia)
    var b = absExact(ib)

    // number of tailing zeroes = power of 2 present
    val a0 = numberOfTrailingZeros(a)
    val b0 = numberOfTrailingZeros(b)
    // extract the the number of trailing zeroes common to them
    val commonPower = math.min(a0, b0)

    // make them odd
    a >>>= a0
    b >>>= b0
    while (a != b) {
      if (a > b) {
        a -= b
        a >>>= numberOfTrailingZeros(a)
      }
      else {
        b -= a
        b >>>= numberOfTrailingZeros(b)
      }
    }
    a << commonPower // multiply back the common power of 2

  }

  def absExact(a: Long): Long = {
    if (a == Long.MinValue) throw new ArithmeticException(s"abs(${Long.MinValue}) overflow")
    math.abs(a)
  }

  case class Rational(numerator: Long, denominator: Long) extends Ordered[Rational] {

    assert(denominator > 0)

    import java.lang.Math.{addExact, multiplyExact}

    def -(r: Rational): Rational = this + -r

    def +(r: Rational): Rational = {
      val num = addExact(multiplyExact(numerator, r.denominator), multiplyExact(r.numerator, denominator))
      val dem = multiplyExact(denominator, r.denominator)
      Rational(num, dem)
    }

    def unary_-(): Rational = new Rational(-numerator, denominator)

    def /(r: Rational): Rational = this * r.inverse

    def *(r: Rational) = Rational(multiplyExact(numerator, r.numerator), multiplyExact(denominator, r.denominator))

    def inverse = Rational(denominator, numerator)

    def toDouble: Double = numerator.toDouble / denominator

    def abs: Rational = new Rational(absExact(numerator), denominator)

    override def toString: String = if (denominator == 1) numerator.toString else s"$numerator/$denominator"

    override def compare(that: Rational): Int = {
      multiplyExact(numerator, that.denominator).compare(multiplyExact(that.numerator, denominator))
    }
  }

  object Rational {
    val zero = new Rational(0, 1)

    def apply(num: Long, den: Long = 1): Rational = {
      if (num == 0) {
        zero
      } else {
        val gcd = cspom.util.Math.gcd(num, den)
        if (den < 0) {
          new Rational(-num / gcd, -den / gcd)
        } else {
          new Rational(num / gcd, den / gcd)
        }
      }
    }
  }


}