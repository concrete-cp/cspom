package cspom.variable;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.Random;

import org.junit.Before;
import org.junit.Test;

public final class ConstantTest {

    private Constant<Integer> intConstant;
    private Constant<Double> doubleConstant;

    private static final Random RAND = new Random();
    private static final int TEST_CONSTANT_INT = RAND.nextInt();
    private static final double TEST_CONSTANT_DOUBLE = RAND.nextDouble();

    @Before
    public void setUp() {
        intConstant = new Constant<Integer>(TEST_CONSTANT_INT);
        doubleConstant = new Constant<Double>(TEST_CONSTANT_DOUBLE);
    }

    @Test
    public void testEquals() {
        assertEquals(intConstant, new Constant<Integer>(TEST_CONSTANT_INT));
        assertEquals(doubleConstant, new Constant<Double>(TEST_CONSTANT_DOUBLE));
        assertFalse(intConstant.equals(new Constant<Double>(
                (double) TEST_CONSTANT_INT)));
    }

    @Test
    public void testHashCode() {
        assertEquals(intConstant.hashCode(), new Constant<Integer>(
                TEST_CONSTANT_INT).hashCode());
        assertEquals(doubleConstant.hashCode(), new Constant<Double>(
                TEST_CONSTANT_DOUBLE).hashCode());
    }

    @Test
    public void testGetValue() {
        assertEquals(intConstant.getValue(), Integer.valueOf(TEST_CONSTANT_INT));
        assertEquals(doubleConstant.getValue(), Double
                .valueOf(TEST_CONSTANT_DOUBLE));
    }
}
