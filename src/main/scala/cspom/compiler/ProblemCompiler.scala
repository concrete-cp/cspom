package cspom.compiler;

import scala.collection.mutable.LinkedHashMap
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.Statistic
import cspom.variable.CSPOMExpression
import scala.collection.mutable.HashMap

/**
 * This class implements some known useful reformulation rules.
 *
 * @author vion
 *
 */
final class ProblemCompiler(
  private val problem: CSPOM,
  private val constraintCompilers: Seq[ConstraintCompiler]) {

  private def compile() {
    ProblemCompiler.compileTime -= System.nanoTime()

    val compilers = new HashMap[ConstraintCompiler, Set[CSPOMConstraint]]()

    val allConstraints = problem.constraints.toSet
    for (cc <- constraintCompilers) {
      compilers(cc) = allConstraints
    }

    while (compilers.nonEmpty) {
      val (compiler, constraints) = compilers.head
      if (constraints.isEmpty) {
        compilers -= compiler
      } else {
        val constraint = constraints.head

        compilers(compiler) -= constraint

        ProblemCompiler.matches += 1

        for (data <- compiler.mtch(constraint, problem)) {
          ProblemCompiler.compiles += 1
          val delta = compiler.compile(constraint, problem, data)

          //println(compiler + " : " + constraint + " -> " + delta)

          for ((cc, queue) <- compilers) {
            compilers(cc) = queue -- delta.removed
          }

          val enqueue = delta.altered.flatMap(problem.constraints)
          for (cc <- constraintCompilers) {
            if (cc eq compiler) {
              compilers(cc) = compilers.getOrElse(cc, Set()) ++ enqueue.filter(_ ne constraint)
            } else {
              compilers(cc) = compilers.getOrElse(cc, Set()) ++ enqueue
            }
          }
        }
      }
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
    new ProblemCompiler(problem, compilers).compile();
  }

  @Statistic
  var matches = 0

  @Statistic
  var compiles = 0

  @Statistic
  var compileTime = 0L

}

