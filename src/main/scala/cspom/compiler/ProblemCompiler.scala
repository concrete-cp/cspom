package cspom.compiler;

import scala.collection.mutable.LinkedHashMap
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.Statistic
import cspom.variable.CSPOMExpression
import scala.collection.mutable.HashMap
import cspom.StatisticsManager

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
    ProblemCompiler.compileTime -= System.nanoTime()

    val toCompile = Array.ofDim[QueueSet[CSPOMConstraint]](constraintCompilers.size)

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

            val enqueue = delta.altered.view.flatMap(problem.constraints)

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
    ProblemCompiler.compileTime += System.nanoTime()
    //    println(problem)
    //    ???
    //    /* Removes disconnected auxiliary variables */
    //    problem.variables.filter { v =>
    //      v.params.contains("var_is_introduced") && problem.constraints(v).isEmpty
    //    }.foreach(problem.removeVariable)

  }

  private def compile(compiler: ConstraintCompiler, constraint: CSPOMConstraint): Set[CSPOMExpression] = {
    ProblemCompiler.matches += 1
    compiler.mtch(constraint, problem).map { data =>
      ProblemCompiler.compiles += 1
      val delta = compiler.compile(constraint, problem, data)
      delta.altered
    } getOrElse {
      Set()
    }
  }
}

object ProblemCompiler {
  def compile(problem: CSPOM, compilers: Seq[ConstraintCompiler]) {
    new ProblemCompiler(problem, compilers.toIndexedSeq).compile();
  }

  val statistics = new StatisticsManager()
  statistics.register("problemcompiler", this)

  @Statistic
  var matches = 0

  @Statistic
  var compiles = 0

  @Statistic
  var compileTime = 0L

}

