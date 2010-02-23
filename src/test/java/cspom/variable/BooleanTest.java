package cspom.variable;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;

public final class BooleanTest {

    @Test
    public void testValueOf() {
        assertSame(BooleanDomain.TRUE, BooleanDomain.valueOf(true));
        assertSame(BooleanDomain.FALSE, BooleanDomain.valueOf(false));
    }

    @Test
    public void testIsConstant() {
        assertTrue(BooleanDomain.TRUE.isConstant());
        assertTrue(BooleanDomain.FALSE.isConstant());
        assertFalse(BooleanDomain.DOMAIN.isConstant());
    }

    @Test
    public void testToString() {
        assertEquals("true", BooleanDomain.TRUE.toString());
    }

    @Test
    public void testGetBoolean() {
        assertSame(BooleanDomain.TRUE.getBoolean(), true);
        assertSame(BooleanDomain.FALSE.getBoolean(), false);
    }

    @Test(expected = IllegalStateException.class)
    public void testGetBooleanDomain() {
        BooleanDomain.DOMAIN.getBoolean();
    }

    @Test
    public void testGetValues() {
        assertEquals(BooleanDomain.DOMAIN.getValues(), Arrays.asList(false, true));
    }

}
