package cspom.compiler;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;

import cspom.DuplicateVariableException;
import cspom.Problem;
import cspom.compiler.PredicateScanner.Node;
import cspom.constraint.Constraint;
import cspom.constraint.FunctionalConstraint;
import cspom.variable.Constant;
import cspom.variable.UnknownDomain;
import cspom.variable.Variable;

public class ConstraintCompiler {

	final private Problem mainProblem;

	public ConstraintCompiler(Problem mainProblem) {
		this.mainProblem = mainProblem;
	}

	public Problem compile(String expression) throws ParseException {
		final Node root = PredicateScanner.scan(expression);
		final Problem problem = new Problem(expression);

		Variable result = addToProblem(root, problem);
		result.setRoot();

		return problem;
	}

	private Variable addToProblem(Node node, Problem problem)
			throws ParseException {
		if (node.isLeaf()) {
			return addVariable(node, problem);
		}
		Variable result = new Variable(new UnknownDomain());
		try {
			problem.addVariable(result);
		} catch (DuplicateVariableException e) {
			throw new IllegalStateException(e);
		}
		Collection<Variable> operands = new ArrayList<Variable>();
		for (Node n = node.getChild(); n != null; n = n.getSibling()) {
			operands.add(addToProblem(n, problem));
		}
		Constraint constraint = new FunctionalConstraint(result, node
				.getOperator(), operands.toArray(new Variable[operands.size()]));
		problem.addConstraint(constraint);
		return result;
	}

	private Variable addVariable(Node node, Problem problem) {
		Variable existing = problem.getVariable(node.getOperator());
		if (existing != null) {
			return existing;
		}

		existing = mainProblem.getVariable(node.getOperator());
		if (existing != null) {
			try {
				problem.addVariable(existing);
			} catch (DuplicateVariableException e) {
				throw new IllegalStateException(e);
			}
			return existing;
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
