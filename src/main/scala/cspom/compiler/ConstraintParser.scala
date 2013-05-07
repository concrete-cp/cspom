package cspom.compiler;

import java.util.LinkedList
import java.util.List
import scala.collection.JavaConversions
import cspom.constraint.FunctionalConstraint
import cspom.constraint.GeneralConstraint
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import scala.collection.mutable.Stack
import cspom.constraint.Predicate
import cspom.variable.AuxVar

final class ConstraintParser(private val problem: CSPOM) {

  def split(expression: String) {
    val root = PredicateScanner.scan(expression);

    require(!root.isLeaf, "Constraint expected");

    problem.addConstraint(new GeneralConstraint(
      new Predicate(root.operator.get, root.parameters),
      root.child.toList.flatMap(_.siblings).map(addToProblem(_)).toList));
  }

  private def addToProblem(node: PredicateNode): CSPOMVariable = {
    if (node.isLeaf) addVariable(node);
    else {
      val result = new AuxVar();
      problem.addVariable(result);

      problem.addConstraint(new FunctionalConstraint(
        result,
        new Predicate(node.operator.get, node.parameters),
        node.child.get.siblings map { addToProblem(_) } toList));

      result;
    }
  }

  def addVariable(node: PredicateNode) = {
    problem.variable(node.operator.get) match {
      case Some(v) => v
      case None => {
        assume(node.isInteger, node + " is not a valid leaf")
        val newVariable = CSPOMVariable.constant(node.operator.get.toInt);
        problem.addVariable(newVariable);
        newVariable;
      }
    }
  }
}
