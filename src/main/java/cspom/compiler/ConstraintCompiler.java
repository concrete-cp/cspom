package cspom.compiler;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;

import cspom.CSPOM;
import cspom.DuplicateVariableException;
import cspom.compiler.PredicateScanner.Node;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.variable.CSPOMVariable;
import cspom.variable.DomainType;

public final class ConstraintCompiler {

	private final CSPOM mainProblem;

	public ConstraintCompiler(CSPOM mainProblem) {
		this.mainProblem = mainProblem;
	}

	public CSPOM split(String expression) throws ParseException {
		final Node root = PredicateScanner.scan(expression);
		final CSPOM problem = new CSPOM();

		CSPOMVariable result = addToProblem(root, problem);
		result.setRoot(true);

		return problem;
	}

	private CSPOMVariable addToProblem(Node node, CSPOM problem)
			throws ParseException {
		if (node.isLeaf()) {
			return addVariable(node, problem);
		}
		CSPOMVariable result = new CSPOMVariable(null);
		try {
			problem.addVariable(result);
		} catch (DuplicateVariableException e) {
			throw new IllegalStateException(e);
		}
		Collection<CSPOMVariable> operands = new ArrayList<CSPOMVariable>();
		for (Node n = node.getChild(); n != null; n = n.getSibling()) {
			operands.add(addToProblem(n, problem));
		}
		CSPOMConstraint constraint = new FunctionalConstraint(result, node
				.getOperator(), operands.toArray(new CSPOMVariable[operands
				.size()]));
		problem.addConstraint(constraint);
		return result;
	}

	private CSPOMVariable addVariable(Node node, CSPOM problem) {
		CSPOMVariable existing = problem.getVariable(node.getOperator());
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

		final CSPOMVariable newVariable;
		if (node.isIdentifier()) {
			newVariable = new CSPOMVariable(null);
		} else if (node.isInteger()) {
			newVariable = new CSPOMVariable(Integer
					.parseInt(node.getOperator()));
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
