package cspom.variable;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

public final class ExtensiveDomainTest {

    private ExtensiveDomain<Integer> intDomain;

    private ExtensiveDomain<?> anyDomain;

    @Before
    public void setUp() {
        intDomain = new ExtensiveDomain<Integer>(1, 7, 9);
        anyDomain = new ExtensiveDomain<Object>(8.9d, 1, intDomain);
    }

    @Test
    public void testGetValues() {
        assertEquals(intDomain.getValues(), Arrays.asList(1, 7, 9));
    }

    @Test
    public void testEquals() {
        assertEquals(intDomain, new ExtensiveDomain<Integer>(Arrays.asList(1,
                7, 9)));
    }

    @Test
    public void testToString() {
        assertEquals(intDomain.toString(), "[1, 7, 9]");
        assertEquals(anyDomain.toString(), "[8.9, 1, [1, 7, 9]]");
    }
}
