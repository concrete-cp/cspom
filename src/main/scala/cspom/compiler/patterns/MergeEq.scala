package cspom.compiler.patterns;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;

import com.google.common.collect.Iterables;

import scala.collection.JavaConversions;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.CSPOMDomain;
import cspom.variable.CSPOMVariable;

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
final class MergeEq(private val problem: CSPOM,
  private val constraints: Deque[CSPOMConstraint]) extends ConstraintCompiler {

  override def compile(constraint: CSPOMConstraint) {

    constraint match {
      case c: GeneralConstraint if c.description == "eq" => {
        val (auxVars, fullVars) = c.scope.partition { _.auxiliary }

        if (auxVars.isEmpty) {
          return ;
        }

        problem.removeConstraint(c)

        /*
         * Generate a new all-equal constraint if more than one variable
         * remains.
         */
        if (fullVars.size > 1) {
          val newConstraint = new GeneralConstraint(description = "eq", scope = fullVars);
          constraints.add(newConstraint)
          problem.addConstraint(newConstraint)
        }

        /*
         * Update the constraints of the problem
         */
        val refVar = if (fullVars.isEmpty) auxVars.head else fullVars.head

        for (aux <- auxVars if aux != refVar) {
          merge(aux.asInstanceOf[CSPOMVariable[AnyRef]], refVar.asInstanceOf[CSPOMVariable[AnyRef]])
        }

      }
    }
  }

  private def mergeDomain[T](d0: CSPOMDomain[T], d1: CSPOMDomain[T]) = {
    if (d0 == null) {
      d1;
    } else if (d1 == null) {
      d0;
    } else
      d0.intersect(d1);
  }

  private def merge[T](merged: CSPOMVariable[T], variable: CSPOMVariable[T]) {
    assume(merged != variable)

    variable.domain = mergeDomain(merged.domain, variable.domain);

    for (c <- merged.constraints) {
      problem.removeConstraint(c);
      problem.addConstraint(c.replacedVar(merged, variable));
    }
    problem.removeVariable(merged);
  }

}
