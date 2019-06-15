package cspom.compiler

import com.typesafe.scalalogging.LazyLogging
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler._
import cspom.util.IntervalsArithmetic.Arithmetics
import cspom.util.{Finite, IntInterval}
import cspom.variable._

import scala.util.{Failure, Try}

object SumDomains extends VariableCompiler("sum") with LazyLogging {

  def compiler(c: CSPOMConstraint[_]) = throw new IllegalStateException

  override def compilerWEntail(c: CSPOMConstraint[_]): (Seq[(CSPOMExpression[Any], SimpleExpression[Any])], Boolean) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, _, _) =>

      val (iargs, coef, result, mode) = SumSE.readCSPOM(c)

      if (iargs.forall(_.fullyDefined)) {
        (Seq(), false)
      } else {

        val initBound = mode match {
          case "le" => IntInterval.atMost(result)
          case "lt" => IntInterval.atMost(result - 1)
          case "eq" => IntInterval.singleton(result)
          case "ne" => IntInterval.all
        }

        val coefspan = (iargs lazyZip coef).map((a, c) => IntExpression.span(a) * Finite(c))

        val filt = for {
          i <- iargs.indices
          others <- Try(
            iargs.indices
              .filter(_ != i)
              .map(coefspan)
              .foldLeft(initBound)(_ - _))
            .recoverWith {
              case e: Exception =>
                logger.warn(s"$e when computing bounds of $c")
                Failure(e)
            }
            .toOption
        } yield {
          iargs(i) -> reduceDomain(iargs(i), others / coef(i))
        }

        val entailed = filt.map(_._2).count(_.searchSpace > 1) <= 1

        (filt, entailed)

      }

    case CSPOMConstraint(r, _, _, _) => (Seq(r -> BoolExpression.coerce(r)), false)


    // case _ => (Seq(), false)
  }
}
