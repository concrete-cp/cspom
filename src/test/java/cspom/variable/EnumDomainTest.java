package cspom.variable;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

public final class EnumDomainTest {

    private static enum EnumTest {
        A, B, C
    }

    private EnumDomain<EnumTest> domain;

    @Before
    public void setUp() {
        domain = EnumDomain.getEnumDomain(EnumTest.class);
    }

    @Test
    public void testGetEnumDomain() {
        assertEquals(domain, EnumDomain.getEnumDomain(EnumTest.class));
        assertSame(domain, EnumDomain.getEnumDomain(EnumTest.class));
    }

    @Test
    public void testGetEnumConstants() {
        assertEquals(domain.getValues(), Arrays.asList(EnumTest.A, EnumTest.B,
                EnumTest.C));
    }

    @Test
    public void testToString() {
        assertEquals(domain.toString(), "[A, B, C]");
    }
}
