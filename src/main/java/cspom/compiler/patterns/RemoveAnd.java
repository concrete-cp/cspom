package cspom.compiler.patterns;

import java.util.Deque;

import scala.collection.JavaConversions;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.CSPOMVariable;
import cspom.variable.TrueDomain$;

public final class RemoveAnd implements ConstraintCompiler {

    private final CSPOM problem;
    private final Deque<CSPOMConstraint> constraints;

    public RemoveAnd(final CSPOM problem,
            final Deque<CSPOMConstraint> constraints) {
        this.problem = problem;
        this.constraints = constraints;
    }

    @Override
    public void compile(final CSPOMConstraint constraint) {
        if ("and".equals(constraint.description())
                && constraint instanceof GeneralConstraint) {
            for (CSPOMVariable<?> v : JavaConversions.asJavaIterable(constraint.scope())) {
                v.domain_$eq(TrueDomain$.MODULE$);
                for (CSPOMConstraint c : JavaConversions.asJavaIterable(v.constraints())) {
                    if (c != constraint) {
                        constraints.add(c);
                    }
                }
            }
            problem.removeConstraint(constraint);

        }
    }

}
