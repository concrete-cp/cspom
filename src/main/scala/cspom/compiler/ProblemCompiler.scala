package cspom.compiler;

import scala.collection.mutable.LinkedHashMap
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.Statistic
import cspom.variable.CSPOMExpression
import scala.collection.mutable.HashMap
import cspom.StatisticsManager
import cspom.TimedException

/**
 * This class implements some known useful reformulation rules.
 *
 * @author vion
 *
 */
final class ProblemCompiler(
  private val problem: CSPOM,
  private val constraintCompilers: IndexedSeq[ConstraintCompiler]) {

  private def compile() {
    val toCompile = Array.ofDim[QueueSet[CSPOMConstraint[Any]]](
      constraintCompilers.size)

    var changed = true
    var first = true

    while (changed) {
      changed = false
      for (i <- toCompile.indices) {

        val compiler = constraintCompilers(i)

        if (first) {
          toCompile(i) = new QueueSet(problem.constraints)
        }

        while (toCompile(i).nonEmpty) {

          val constraint = toCompile(i).dequeue()
          ProblemCompiler.matches += 1
          //println(compiler, constraint.id)

          for (data <- compiler.mtch(constraint, problem)) {
            ProblemCompiler.compiles += 1
            changed = true
            val delta = compiler.compile(constraint, problem, data)

            //println(compiler + " : " + constraint + " -> " + delta)

            lazy val enqueue = delta.altered.iterator.flatMap(problem.constraints).toList

            for (j <- if (first) { 0 to i } else { toCompile.indices }) {
              toCompile(j).removeAll(delta.removed)
              if (j != i || compiler.selfPropagation) {
                toCompile(j).enqueueAll(enqueue)
              }
            }
          }
          toCompile(i).remove(constraint)
        }

      }
      first = false
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

