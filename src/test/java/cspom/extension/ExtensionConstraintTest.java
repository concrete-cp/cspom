package cspom.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

public class ExtensionConstraintTest {

    private Extension<Integer> extension;

    @Before
    public void setUp() throws Exception {
        extension = new Extension<Integer>(3, false);
        extension.addTuple(2, 5, 5);
        extension.addTuple(3, 5, 5);
    }

    @Test
    public void testEvaluate() {
        assertFalse(extension.evaluate(1, 2, 3));
        assertTrue(extension.evaluate(2, 5, 5));
    }

    @Test
    public void testTuplesToString() {
        assertEquals("2 5 5|3 5 5", extension.tupleString());
    }

    @Test
    public void testAddTuple() {
        extension.addTuple(2, 5, 5);
        assertEquals(2, extension.getNbTuples());
    }
}
