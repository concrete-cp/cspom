package cspom.extension;


import java.util.Arrays;

import org.junit.Before;

import cspom.extension.Extension;
import cspom.extension.ExtensionConstraint;
import cspom.variable.Domain;
import cspom.variable.ExtensiveDomain;
import cspom.variable.Variable;

public class ExtensionConstraintTest {

	@Before
	public void setUp() throws Exception {
	}


	public static void main(String[] args) {
		Extension relation = new Extension("test", 3, 4, true, new Number[][] {
				{ 1, 2, 3 }, { 2, 3, 4 }, { 3, 4, 5 }, { 4, 5, 6 } });

		Domain domain = new ExtensiveDomain("d1", Arrays.asList(new Number[] {
				1, 2, 3, 4, 5, 6 }));

		Variable v1 = new Variable("v1", domain);
		Variable v2 = new Variable("v2", domain);
		Variable v3 = new Variable("v3", domain);

		ExtensionConstraint constraint = new ExtensionConstraint("c1", Arrays
				.asList(new Variable[] { v1, v2, v3 }), relation);

		System.out.println(constraint);
		System.out.println(constraint.getRelation().tupleString());

		ExtensionConstraint c2 = constraint.standardize(Arrays
				.asList(new Variable[] { v2, v3, v1 }));

		System.out.println(c2);
		System.out.println(c2.getRelation().tupleString());

	}
}
