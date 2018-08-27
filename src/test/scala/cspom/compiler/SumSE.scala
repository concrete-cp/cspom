package cspom.compiler

import cspom.CSPOM.seq2CSPOMSeq
import cspom.CSPOMConstraint
import cspom.variable._


object SumSE extends ACCSE[IntPair, Int] {


  def readCSPOM(constraint: CSPOMConstraint[_]): (IndexedSeq[SimpleExpression[Any]], Seq[Int], Int, String) = {
    require(constraint.arguments.lengthCompare(3) == 0)
    val IntExpression.constSeq(coefs) = constraint.arguments(0)
    val SimpleExpression.simpleSeq(vars) = constraint.arguments(1)
    val CSPOMConstant(c: Int) = constraint.arguments(2) //map cspom2concreteVar

    // For bool2int optimization
    val constant = c

    val mode = constraint.getParam[String]("mode").get

    (vars, coefs, constant, mode)
  }

  override def constraintToArgs(c: CSPOMConstraint[_]): IndexedSeq[Arg] = {
    val (vars, coefs, _, _) = readCSPOM(c)
    vars zip coefs
  }

  def filter(c: CSPOMConstraint[_]): Boolean = c.function == 'sum

  override def define(pairexp: IntPair, aux: CSPOMVariable[_]): (Arg, CSPOMConstraint[_]) = {
    val definition = CSPOMConstraint('sum)(
      Seq(CSPOMConstant(pairexp.k1), CSPOMConstant(pairexp.k2), CSPOMConstant(-1)),
      Seq(pairexp.v1, pairexp.v2, aux),
      CSPOMConstant(0))
      .withParam("mode" -> "eq")

    val arg = (aux, 1)

    (arg, definition)
  }

  override def toString(pairexp: IntPair, dn: CSPOMExpression[_] => String): String =
    s"${pairexp.k1}·${pairexp.v1.toString(dn)} + ${pairexp.k2}·${pairexp.v2.toString(dn)}"

  override def toString(pairexp: Arg, dn: CSPOMExpression[_] => String): String =
    s"${pairexp._2}·${pairexp._1.toString(dn)}"

  def replace(pairexp: IntPair, arg: Arg, constraint: Args): Boolean = {

    // Obtain actual coefficients, use options because some variables might already have been removed
    val r = for {
      k1 <- constraint.get(pairexp.v1)
      k2 <- constraint.get(pairexp.v2)
      k = k1 / pairexp.k1
      // Check also correction for second coef. May be wrong if there are several occurrences of the same variable in the scope!
      if k2 == pairexp.k2 * k
    } yield {
      // Integer division should be correct
      assert(pairexp.k1 * k == k1)

      // remove subexpression
      constraint -= pairexp.v1
      constraint -= pairexp.v2

      // Replace pairexp with aux variable
      constraint += arg.copy(_2 = k)
      //
    }
    r.isDefined
  }

  def pair(a1: Arg, a2: Arg): IntPair = {

    val (v1, k1) = a1
    val (v2, k2) = a2

    val gcd = IntPair.gcd(math.abs(k1), math.abs(k2))

    if (k1 < k2) {
      CoefPair(k1 / gcd, v1, k2 / gcd, v2)
    } else if (k1 > k2) {
      CoefPair(k2 / gcd, v2, k1 / gcd, v1)
    } else {
      assert(gcd == math.abs(k1), s"Coefficients $k1 and $k2 are equal, should also be equal to gcd $gcd")
      if (v1.hashCode < v2.hashCode) {
        EqualPair(v1, v2)
      } else {
        EqualPair(v2, v1)
      }
    }
  }

  override def argsToConstraint(original: CSPOMConstraint[_], args: Args): CSPOMConstraint[_] = {
    val (_, _, constant, mode) = readCSPOM(original)
    val (vars, coefs) = args.unzip
    CSPOMConstraint(original.result)('sum)(seq2CSPOMSeq(coefs.map(CSPOMConstant(_))), seq2CSPOMSeq(vars), CSPOMConstant(constant)) withParam ("mode" -> mode)
  }
}

object IntPair {
  def even(a: Int): Boolean = (a & 0x1) == 0

  def gcd(ia: Int, ib: Int): Int = {
    var d = 0
    var a = ia
    var b = ib
    while (even(a) && even(b)) {
      a /= 2
      b /= 2
      d += 1
    }
    while (a != b) {
      if (even(a)) {
        a /= 2
      } else if (even(b)) {
        b /= 2
      } else if (a > b) {
        a = (a - b) / 2
      } else {
        b = (b - a) / 2
      }
    }

    a * (0x1 << d)
  }
}

sealed trait IntPair {
  def k1: Int

  def k2: Int

  def v1: CSPOMExpression[_]

  def v2: CSPOMExpression[_]
}


case class EqualPair(v1: CSPOMExpression[_], v2: CSPOMExpression[_]) extends IntPair {
  def k1 = 1

  def k2 = 1
}

case class CoefPair(k1: Int, v1: CSPOMExpression[_], k2: Int, v2: CSPOMExpression[_]) extends IntPair