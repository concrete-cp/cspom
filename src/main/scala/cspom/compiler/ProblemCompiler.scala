package cspom.compiler;

import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable

/**
 * This class implements some known useful reformulation rules.
 *
 * @author vion
 *
 */
final class ProblemCompiler(
  private val problem: CSPOM,
  private val constraintCompilers: Seq[ConstraintCompiler]) {

  private val constraints = new QueueSet[CSPOMConstraint]()
  private val compilers = new QueueSet[ConstraintCompiler]()

  private def compile() {
    val compilers = new QueueSet[ConstraintCompiler]()

    compilers.enqueue(constraintCompilers: _*)

    while (compilers.nonEmpty) {
      val compiler = compilers.dequeue()
      val constraints = new QueueSet[CSPOMConstraint]()
      constraints.enqueue(problem.constraints.toSeq: _*)
      var ch = false
      while (constraints.nonEmpty) {
        val constraint = constraints.dequeue()
        println(constraint)
        for (data <- compiler.mtch(constraint, problem)) {
          val delta = compiler.compile(constraint, problem, data)

          constraints.remove(delta.removed: _*)
          for (v <- delta.altered) {
            constraints.enqueue(problem.constraints(v): _*)
          }
          ch |= delta.nonEmpty
        }

      }

      if (ch) {
        compilers.enqueue(constraintCompilers.filterNot(_ == compiler): _*)
      }
    }

    /* Removes disconnected auxiliary variables */
    problem.variables.filter { v =>
      v.params.contains("var_is_introduced") && problem.constraints(v).isEmpty
    }.foreach(problem.removeVariable)

  }

  private def hasChanged[A](l: Option[A], f: A => Boolean) = l.map(f).getOrElse(false)

}

object ProblemCompiler {
  def compile(problem: CSPOM, compilers: Seq[ConstraintCompiler]) {
    new ProblemCompiler(problem, compilers).compile();
  }

}

