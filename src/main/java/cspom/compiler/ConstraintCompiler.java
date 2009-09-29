package cspom.compiler;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import cspom.DuplicateVariableException;
import cspom.Problem;
import cspom.compiler.PredicateScanner.Node;
import cspom.constraint.Constraint;
import cspom.constraint.FunctionalConstraint;
import cspom.variable.Constant;
import cspom.variable.UnknownDomain;
import cspom.variable.Variable;

public class ConstraintCompiler {

	public static Problem compile(String expression,
			Map<String, Variable> existingVariables) throws ParseException {
		final Node root = PredicateScanner.scan(expression);
		final Problem problem = new Problem(expression);

		Variable result = addToProblem(root, problem, existingVariables);
		result.setRoot();

		return problem;
	}

	private static Variable addToProblem(Node node, Problem problem,
			Map<String, Variable> existingVariables) throws ParseException {
		if (node.isLeaf()) {
			return addVariable(node, problem, existingVariables);
		}
		Variable result = new Variable(new UnknownDomain());
		try {
			problem.addVariable(result);
		} catch (DuplicateVariableException e) {
			throw new IllegalStateException(e);
		}
		Collection<Variable> operands = new ArrayList<Variable>();
		for (Node n = node.getChild(); n != null; n = n.getSibling()) {
			operands.add(addToProblem(n, problem, existingVariables));
		}
		Constraint constraint = new FunctionalConstraint(result, node
				.getOperator(), operands.toArray(new Variable[operands.size()]));
		problem.addConstraint(constraint);
		return result;
	}

	private static Variable addVariable(Node node, Problem problem,
			Map<String, Variable> existingVariables) {
		Variable existing = problem.getVariable(node.getOperator());
		if (existing != null) {
			return existing;
		}

		existing = existingVariables.get(node.getOperator());
		if (existing != null) {
			try {
				problem.addVariable(existing);
			} catch (DuplicateVariableException e) {
				throw new IllegalStateException(e);
			}
		}

		final Variable newVariable;
		if (node.isIdentifier()) {
			newVariable = new Variable(new UnknownDomain());
		} else if (node.isInteger()) {
			newVariable = new Constant(Integer.parseInt(node.getOperator()));
		} else {
			throw new IllegalStateException();
		}
		try {
			problem.addVariable(newVariable);
		} catch (DuplicateVariableException e) {
			System.err.println(problem);
			System.err.println(newVariable);
			throw new IllegalStateException(e);
		}
		return newVariable;
	}
}
