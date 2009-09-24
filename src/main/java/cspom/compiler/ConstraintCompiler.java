package cspom.compiler;

import java.text.ParseException;
import java.util.Map;

import cspom.Problem;
import cspom.compiler.PredicateScanner.Node;
import cspom.variable.Variable;

public class ConstraintCompiler {

	public static Problem compile(String expression,
			Map<String, Variable> variables) throws ParseException {
		final Node root = PredicateScanner.scan(expression);
		final Problem problem = new Problem(expression);

		addToProblem(root, problem);

		return problem;
	}

	private static void addToProblem(Node root, Problem problem)
			throws ParseException {
		if (root.isIdentifier() && !root.isLeaf() || root.isLeaf()
				&& !root.isIdentifier()) {
			throw new ParseException("Identifier expected", 0);
		}

	}
}
