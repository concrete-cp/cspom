package cspom.compiler;

import scala.collection.mutable.LinkedHashMap
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.Statistic
import cspom.variable.CSPOMExpression
import scala.collection.mutable.HashMap
import cspom.StatisticsManager
import cspom.TimedException
import cspom.VariableNames
import com.typesafe.scalalogging.LazyLogging

sealed trait Reason
case class ConstraintReason(constraint: CSPOMConstraint[_]) extends Reason
case class ExpressionReason(expression: CSPOMExpression[_]) extends Reason

/**
 * This class implements some known useful reformulation rules.
 *
 * @author vion
 *
 */
final class ProblemCompiler(
  private val problem: CSPOM,
  private val constraintCompilers: IndexedSeq[ConstraintCompiler]) extends LazyLogging {

  val vn = new VariableNames(problem)

  private def compile() {
    val toCompile = Array.ofDim[QueueSet](
      constraintCompilers.size)

    val constraints = new HashMap[Int, CSPOMConstraint[_]]
    //val reasons = new HashMap[Int, Set[Reason]]

    for (c <- problem.constraints) {
      constraints.put(c.id, c)
    }

    var changed = true
    var first = true

    while (changed) {
      changed = false
      for (i <- toCompile.indices) {

        val compiler = constraintCompilers(i)
        logger.info(compiler.toString)
        //println(compiler)
        if (first) {
          toCompile(i) = new QueueSet(constraints.keys)
        }

        while (toCompile(i).nonEmpty) {

          for (constraint <- constraints.get(toCompile(i).dequeue())) {
            val delta = compile(compiler, constraint)

            changed |= delta.nonEmpty

            for (rc <- delta.removed) {
              require(!problem.constraintSet(rc), s"$compiler: $rc is still present")
              constraints.remove(rc.id)
            }

            for (c <- delta.added) {
              require(problem.constraintSet(c), s"$compiler: $c is not present")
              constraints.put(c.id, c)
            }

            val enqueue = delta.added.flatMap(
              _.fullScope).distinct.flatMap(
                problem.constraints(_)).distinct

            for (j <- if (first) { 0 to i } else { toCompile.indices }) {
              if (j != i || compiler.selfPropagation) {
                for (ac <- enqueue if i != j || (ac ne constraint)) {
                  toCompile(j).enqueue(ac.id)
                }
              }
            }

          }

          //toCompile(i).remove(constraint.id)
          //println

        }

      }

      first = false
    }
  }

  def compile(compiler: ConstraintCompiler, constraint: CSPOMConstraint[_]): Delta = {
    require(problem.constraintSet(constraint), {
      val vn = new VariableNames(problem)
      s"${constraint.toString(vn)} not in $problem"
    })
    ProblemCompiler.matches += 1

    compiler.mtch(constraint, problem) match {
      case Some(data) =>
        ProblemCompiler.compiles += 1
        logger.debug(s"$compiler : ${constraint.toString(vn)}")
        compiler.compile(constraint, problem, data)
      case None => Delta()
    }

  }

}

object ProblemCompiler {
  def compile(problem: CSPOM, compilers: Seq[ConstraintCompiler]) {
    val pbc = new ProblemCompiler(problem, compilers.toIndexedSeq)

    val (_, t) = try StatisticsManager.time(pbc.compile())
    catch {
      case e: TimedException =>
        compileTime += e.time
        throw e.getCause()
    }
    compileTime += t
  }

  @Statistic
  var matches = 0

  @Statistic
  var compiles = 0

  @Statistic
  var compileTime = 0.0

}

