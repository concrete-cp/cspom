package cspom.extension;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Test;

import cspom.variable.Domain;
import cspom.variable.ExtensiveDomain;
import cspom.variable.Variable;

public class ExtensionConstraintTest {

	@Test
	public void testExtensionConstraint() {
		Extension relation = new Extension("test", 3, 4, true, new Number[][] {
				{ 1, 2, 3 }, { 2, 3, 4 }, { 3, 4, 5 }, { 4, 5, 6 } });

		Domain domain = new ExtensiveDomain("d1", Arrays.asList(new Number[] {
				1, 2, 3, 4, 5, 6 }));

		Variable v1 = new Variable("v1", domain);
		Variable v2 = new Variable("v2", domain);
		Variable v3 = new Variable("v3", domain);

		ExtensionConstraint constraint = new ExtensionConstraint("c1", Arrays
				.asList(new Variable[] { v1, v2, v3 }), relation);

		assertEquals("c1 ([v1 (d1: [6]), v2 (d1: [6]), v3 (d1: [6])]): test: 3-ary, 4 tuples, supports" ,constraint.toString());
		assertEquals("1 2 3|2 3 4|3 4 5|4 5 6", constraint.getRelation().tupleString());

		ExtensionConstraint c2 = constraint.standardize(Arrays
				.asList(new Variable[] { v2, v3, v1 }));

		assertEquals("c1 ([v2 (d1: [6]), v3 (d1: [6]), v1 (d1: [6])]): test: 3-ary, 4 tuples, supports",c2.toString());
		assertEquals("2 3 1|3 4 2|4 5 3|5 6 4", c2.getRelation().tupleString());

	}
}
