package cspom.variable;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Random;

import org.junit.Before;
import org.junit.Test;

public final class IntIntervalTest {

	private IntInterval intInterval;

	@Before
	public void setUp() throws Exception {
		intInterval = new IntInterval(10, 15);
	}

	@Test
	public void testInterval() {
		assertEquals(10, intInterval.getLb());
		assertEquals(15, intInterval.getUb());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testBadInterval() {
		new IntInterval(15, 10);
	}

	@Test
	public void testEquals() {
		assertEquals(intInterval, new IntInterval(10, 15));
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

	@Test
	public void testValueOf() {
		final IntInterval itvInt = IntInterval.valueOf("10..15");
		assertEquals(intInterval, itvInt);
	}
}
