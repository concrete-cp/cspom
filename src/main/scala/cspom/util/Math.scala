package cspom.util

import java.math.RoundingMode

object Math {
  def checkedAdd(i: Int, j: Int) = {
    val l = i.toLong + j.toLong
    if (l > Int.MaxValue || l < Int.MinValue) {
      throw new ArithmeticException(s"$i + $j: overflow")
    }
    l.toInt
  }

  def checkedSubtract(i: Int, j: Int) = {
    val l = i.toLong - j.toLong
    if (l > Int.MaxValue || l < Int.MinValue) {
      throw new ArithmeticException(s"$i - $j: overflow")
    }
    l.toInt
  }

  def checkedMultiply(i: Int, j: Int) = {
    val l = i.toLong * j.toLong
    if (l > Int.MaxValue || l < Int.MinValue) {
      throw new ArithmeticException(s"$i * $j: overflow")
    }
    l.toInt
  }

  /**
   * Returns the result of dividing {@code p} by {@code q}, rounding using the specified
   * {@code RoundingMode}.
   *
   * @throws ArithmeticException if {@code q == 0}, or if {@code mode == UNNECESSARY} and {@code a}
   *         is not an integer multiple of {@code b}
   */
  def divide(p: Int, q: Int, mode: RoundingMode): Int = {
    import RoundingMode._
    if (q == 0) {
      throw new ArithmeticException("/ by zero"); // for GWT
    }
    val div = p / q;
    val rem = p - q * div; // equal to p % q

    if (rem == 0) {
      div;
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
        case UNNECESSARY if (rem == 0) =>
          throw new ArithmeticException("mode was UNNECESSARY, but rounding was necessary");

        case UNNECESSARY | DOWN => false;
        case UP => true
        case CEILING => signum > 0;
        case FLOOR => signum < 0;

        case HALF_EVEN |
          HALF_DOWN |
          HALF_UP =>
          val absRem = math.abs(rem);
          val cmpRemToHalfDivisor = absRem - (math.abs(q) - absRem);
          // subtracting two nonnegative ints can't overflow
          // cmpRemToHalfDivisor has the same sign as compare(abs(rem), abs(q) / 2).
          if (cmpRemToHalfDivisor == 0) { // exactly on the half mark
            mode == HALF_UP || (mode == HALF_EVEN & (div & 1) != 0);
          } else {
            cmpRemToHalfDivisor > 0; // closer to the UP value
          }
        case _ =>
          throw new AssertionError();
      }
      if (increment) div + signum else div;
    }
  }
}