package cspom.compiler;

import java.util.Collection;
import java.util.LinkedList;

import cspom.CSPOM;
import cspom.DuplicateVariableException;
import cspom.compiler.PredicateScanner.Node;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.CSPOMVariable;

public final class ConstraintParser {

    private final CSPOM problem;

    public ConstraintParser(final CSPOM problem) {
        this.problem = problem;
    }

    public void split(final String expression) throws PredicateParseException {
        final Node root = PredicateScanner.scan(expression);

        if (root.isLeaf()) {
            throw new IllegalArgumentException("Constraint expected");
        }
        final Collection<CSPOMVariable> operands = new LinkedList<CSPOMVariable>();
        for (Node n = root.getChild(); n != null; n = n.getSibling()) {
            operands.add(addToProblem(n));
        }
        problem.addConstraint(new GeneralConstraint(root.getOperator(), root
                .getParameters(), operands.toArray(new CSPOMVariable[operands
                .size()])));
    }

    private CSPOMVariable addToProblem(final Node node)
            throws PredicateParseException {
        if (node.isLeaf()) {
            return addVariable(node, problem);
        }

        final CSPOMVariable result = new CSPOMVariable();
        result.setAuxiliary(true);
        try {
            problem.addVariable(result);
        } catch (DuplicateVariableException e) {
            throw new IllegalStateException(e);
        }
        final Collection<CSPOMVariable> operands = new LinkedList<CSPOMVariable>();
        for (Node n = node.getChild(); n != null; n = n.getSibling()) {
            operands.add(addToProblem(n));
        }
        problem.addConstraint(new FunctionalConstraint(result, node
                .getOperator(), node.getParameters(), operands
                .toArray(new CSPOMVariable[operands.size()])));
        return result;
    }

    private CSPOMVariable addVariable(final Node node, final CSPOM problem) {
        CSPOMVariable existing = problem.getVariable(node.getOperator());
        if (existing != null) {
            return existing;
        }

        existing = problem.getVariable(node.getOperator());
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
            newVariable = new CSPOMVariable();
        } else if (node.isInteger()) {
            newVariable = new CSPOMVariable(Integer
                    .parseInt(node.getOperator()));
            newVariable.setAuxiliary(true);
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
