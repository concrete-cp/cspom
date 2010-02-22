package cspom.variable;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;

public final class BooleanTest {

    @Test
    public void testValueOf() {
        assertSame(Boolean.TRUE, Boolean.valueOf(true));
        assertSame(Boolean.FALSE, Boolean.valueOf(false));
    }

    @Test
    public void testIsConstant() {
        assertTrue(Boolean.TRUE.isConstant());
        assertTrue(Boolean.FALSE.isConstant());
        assertFalse(Boolean.DOMAIN.isConstant());
    }

    @Test
    public void testToString() {
        assertEquals("true", Boolean.TRUE.toString());
    }

    @Test
    public void testGetBoolean() {
        assertSame(Boolean.TRUE.getBoolean(), true);
        assertSame(Boolean.FALSE.getBoolean(), false);
    }

    @Test(expected = IllegalStateException.class)
    public void testGetBooleanDomain() {
        Boolean.DOMAIN.getBoolean();
    }

    @Test
    public void testGetValues() {
        assertEquals(Boolean.DOMAIN.getValues(), Arrays.asList(false, true));
    }

}
