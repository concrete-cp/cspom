package cspom.compiler;

import java.util.LinkedList;
import java.util.List;

import scala.collection.JavaConversions;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.CSPOMVariable;
import cspom.CSPOM;
import scala.collection.mutable.Stack;

final class ConstraintParser(private val problem: CSPOM) {

  def split(expression: String) {
    val root = PredicateScanner.scan(expression);

    assume(!root.isLeaf, "Constraint expected");

    problem.addConstraint(new GeneralConstraint(
        root.operator, 
        root.parameters, 
        root.child.siblings map { addToProblem(_) } toList));
  }

  private def addToProblem(node: PredicateNode): CSPOMVariable = {
    if (node.isLeaf) {
      addVariable(node);
    } else {
      val result = CSPOMVariable.aux();
      problem.addVariable(result);

      problem.addConstraint(new FunctionalConstraint(
        result, 
        node.operator, 
        node.parameters,
        node.child.siblings map { addToProblem(_) } toList));
      
      result;
    }
  }

  def addVariable(node: PredicateNode) = {
    problem.variable(node.operator) match {
      case Some(v) => v
      case None => {
        assume(node.isInteger, node + " is not a valid leaf")
        val newVariable = CSPOMVariable.constant(node.operator.toInt);
        problem.addVariable(newVariable);
        newVariable;
      }
    }
  }
}
