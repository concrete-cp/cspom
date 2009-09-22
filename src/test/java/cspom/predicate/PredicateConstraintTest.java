package cspom.predicate;

import static org.junit.Assert.assertSame;

import java.util.Arrays;

import javax.script.ScriptException;

import org.junit.Before;
import org.junit.Test;

import cspom.predicate.PredicateConstraint;
import cspom.variable.Domain;
import cspom.variable.ExtensiveDomain;
import cspom.variable.Variable;
import cspom.xcsp.Predicate;

public class PredicateConstraintTest {

    private PredicateConstraint constraint;

    private Variable var1, var2;

    @Before
    public void setUp() throws Exception {
        final Predicate predicate = new Predicate("p0", "int X0 int X1",
                "X0<X1");

        final Domain d1 = new ExtensiveDomain("d1", Arrays.asList(new Number[] {
                1, 2, 3, 4, 5, 6 }));

        final Domain d2 = new ExtensiveDomain("d1", Arrays.asList(new Number[] {
                5, 6, 7 }));

        var1 = new Variable("v1", d1);
        var2 = new Variable("v2", d2);

        constraint = new PredicateConstraint("c1", "v1 v2", predicate, var1,
                var2);

    }

    @Test
    public void testEvaluate() throws ScriptException {
        Number[] tuple = new Number[2];
        for (Number i : var1.getDomain().getValues()) {
            tuple[0] = i;
            for (Number j : var2.getDomain().getValues()) {
                tuple[1] = j;
                assertSame("Constraint test", (Integer) i < (Integer) j,
                        constraint.evaluate(tuple));
            }
        }
    }

    @Test
    public void testStandardize() throws ScriptException {
        PredicateConstraint c2 = constraint.standardize(new Variable[] { var2,
                var1 });

        Number[] tuple = new Number[2];
        for (Number i : var1.getDomain().getValues()) {
            tuple[0] = i;
            for (Number j : var2.getDomain().getValues()) {
                tuple[1] = j;
                assertSame("Constraint test", c2.evaluate(tuple), constraint
                        .evaluate(new Number[] { tuple[1], tuple[0] }));
            }
        }
    }
}
