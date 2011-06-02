package cspom.compiler;

import cspom.CSPOM
import cspom.compiler.patterns.AbsDiff
import cspom.compiler.patterns.AllDiff
import cspom.compiler.patterns.ConstraintCompiler
import cspom.compiler.patterns.DeReify
import cspom.compiler.patterns.DiffGe
import cspom.compiler.patterns.MergeEq
import cspom.compiler.patterns.RemoveAnd
import cspom.constraint.CSPOMConstraint
import cspom.variable.CSPOMVariable
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.JavaConversions

/**
 * This class implements some known useful reformulation rules.
 *
 * @author vion
 *
 */
final class ProblemCompiler(private val problem: CSPOM) {

  private val constraints = new Queue[CSPOMConstraint]() {

    /**
     *
     */
    val serialVersionUID = 1L;

    val present = new HashSet[CSPOMConstraint]();

    override def enqueue(e: CSPOMConstraint*) = {
      for (c <- e if (!present.contains(c))) {
        super.enqueue(c)
      }
    }

    override def dequeue() = {
      val constraint = super.dequeue();
      present.remove(constraint);
      constraint;
    }

  };

  private val constraintCompilers = List(
    new RemoveAnd(problem, constraints),
    new MergeEq(problem, constraints),
    new AllDiff(problem),
    new DiffGe(problem),
    new AbsDiff(problem),
    new DeReify(problem, constraints))

  private def compile() {
    problem.constraints.foreach(constraints.enqueue(_));

    while (!constraints.isEmpty) {
      compileConstraint(constraints.dequeue());
    }

    /* Removes single auxiliary variables */
    problem.variables.filter { v =>
      v.auxiliary && v.constraints.isEmpty
    }.foreach(problem.removeVariable)
  }

  private def compileConstraint(constraint: CSPOMConstraint) {
    for (cc <- constraintCompilers) {
      if (!problem.constraints.contains(constraint)) {
        return ;
      }
      cc.compile(constraint);
    }
  }

}

object ProblemCompiler {
  def compile(problem: CSPOM) {
    new ProblemCompiler(problem).compile();
  }
}

