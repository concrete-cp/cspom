package cspom.compiler;

import java.util.LinkedList;
import java.util.List;

import scala.collection.JavaConversions;
import cspom.compiler.PredicateScanner.Node;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.CSPOMVariable;
import cspom.CSPOM;

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
        final List<CSPOMVariable<?>> operands = new LinkedList<CSPOMVariable<?>>();
        for (Node n = root.getChild(); n != null; n = n.getSibling()) {
            operands.add(addToProblem(n));
        }
        problem.addConstraint(new GeneralConstraint(root.getOperator(), root
                .getParameters(), JavaConversions.asScalaBuffer(operands)));
    }

    private CSPOMVariable<?> addToProblem(final Node node)
            throws PredicateParseException {
        if (node.isLeaf()) {
            return addVariable(node, problem);
        }

        final CSPOMVariable<?> result = CSPOMVariable.aux();
        problem.addVariable(result);

        final List<CSPOMVariable<?>> operands = new LinkedList<CSPOMVariable<?>>();
        for (Node n = node.getChild(); n != null; n = n.getSibling()) {
            operands.add(addToProblem(n));
        }
        problem.addConstraint(new FunctionalConstraint(result, node
                .getOperator(), node.getParameters(), JavaConversions
                .asScalaBuffer(operands)));
        return result;
    }

    private static CSPOMVariable<?> addVariable(final Node node,
            final CSPOM problem) {
        final CSPOMVariable<?> existing = problem.variable(node.getOperator());
        if (existing != null) {
            return existing;
        }

        final CSPOMVariable<?> newVariable;
        if (node.isInteger()) {
            newVariable = CSPOMVariable.constant(Integer.parseInt(node
                    .getOperator()));
        } else {
            throw new IllegalStateException();
        }
        problem.addVariable(newVariable);
        return newVariable;
    }
}
