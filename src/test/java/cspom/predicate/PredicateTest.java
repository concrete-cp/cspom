package cspom.predicate;


import static org.junit.Assert.assertSame;

import javax.script.ScriptException;

import org.junit.Before;
import org.junit.Test;

import cspom.xcsp.Predicate;

public class PredicateTest {

	private Predicate predicate;

	@Before
	public void setUp() throws Exception {
		predicate = new Predicate("Test", "int X0", "min(X0, 2) == X0");
	}

	@Test
	public void testEvaluateNumberArray() throws ScriptException {
		// Map<String, Number> values = new HashMap<String, Number>();
		// int[] values = new int[1];
		Number[] values = new Number[1];

		for (int i = 10000; --i >= 0;) {
			values[0] = i;
			// values.put("X0", 11);

			assertSame("Predicate test", predicate.evaluate(values), Math.min(
					i, 2) == i);

		}
	}

}
