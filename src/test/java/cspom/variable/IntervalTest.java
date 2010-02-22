package cspom.variable;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.Random;

import org.junit.Before;
import org.junit.Test;

public final class IntervalTest {

    private static final Random RAND = new Random();

    private Interval<Integer> intInterval;
    private Interval<Double> doubleInterval;

    @Before
    public void setUp() throws Exception {
        intInterval = new Interval<Integer>(10, 15);
        double doubleUb;
        double doubleLb;
        do {
            doubleUb = RAND.nextDouble();
            doubleLb = RAND.nextDouble();
        } while (doubleLb > doubleUb);
        doubleInterval = new Interval<Double>(doubleLb, doubleUb);
    }

    @Test
    public void testInterval() {
        assertEquals(10, intInterval.getLb());
        assertEquals(15, intInterval.getUb());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testBadInterval() {
        new Interval<Integer>(15, 10);
    }

    @Test
    public void testToString() {
        assertEquals("[10..15]", intInterval.toString());
    }

    @Test
    public void testGetValues() {
        assertEquals(Arrays.asList(10, 11, 12, 13, 14, 15), intInterval
                .getValues());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetValuesDouble() {
        doubleInterval.getValues();
    }

}
