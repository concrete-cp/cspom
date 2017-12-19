package cspom.compiler

import cspom.compiler.ConstraintCompiler._
import cspom.variable.{CSPOMConstant, CSPOMExpression}
import cspom.{CSPOM, CSPOMConstraint, UNSATException}


abstract class VariableCompiler(val function: Symbol) extends ConstraintCompiler {

  // Do not use Maps to avoid hash undeterminism
  type A = (Seq[(CSPOMExpression[_], CSPOMExpression[_])], Boolean)

  def compiler(c: CSPOMConstraint[_]): Seq[(CSPOMExpression[_], CSPOMExpression[_])]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM): Option[(Seq[(CSPOMExpression[_], CSPOMExpression[_])], Boolean)] = {
    if (c.function == function && c.fullScope.exists(v => !v.fullyDefined)) {
      val (reductions, entail) = try {
        compilerWEntail(c)
      } catch {
        case e: UNSATException =>
          for {
            v <- c.flattenedScope
            if !v.isInstanceOf[CSPOMConstant[_]]
            n <- problem.deepConstraints(v)
          } {
            logger.debug(n.toString)
          }

          throw new UNSATException(s"$c is inconsistent", e)
      }

      val m = reductions.filter { case (k, v) => k != v }

      assert(m.forall(e => c.flattenedScope.contains(e._1)), s"$c must involve all $m")

      if (m.nonEmpty || entail) {
        logger.debug(s"$c: $m")
        Some((m, entail))

      } else {
        None
      }
    }
    else {
      None
    }
  }

  def compilerWEntail(c: CSPOMConstraint[_]): (Seq[(CSPOMExpression[_], CSPOMExpression[_])], Boolean) = {
    (compiler(c), false)
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val (reductions, entail) = data
    val entailed =
      if (entail) {
        removeCtr(c, problem)
      } else {
        Delta.empty
      }

    reductions.map { case (k, v) => replace(k, v, problem) }.foldLeft(entailed)(_ ++ _)

  }

  //}
  //    var d = Delta()
  //    for ((k, v) <- data) {
  //      d ++= replace(k, v, problem)
  //    }
  //    d
  //  }

  def selfPropagation = true

}
