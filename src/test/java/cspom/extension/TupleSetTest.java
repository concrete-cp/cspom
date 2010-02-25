package cspom.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

public final class TupleSetTest {

    private Set<Integer[]> ts;

    @Before
    public void setUp() throws Exception {
        ts = new TupleSet<Integer>();
        ts.add(new Integer[] { 0, 0 });
        ts.add(new Integer[] { 0, 1 });
        ts.add(new Integer[] { 1, 0 });
    }

    @Test
    public void testContainsTuple() {
        assertTrue(ts.contains(new Integer[] { 0, 1 }));
        assertFalse(ts.contains(new Integer[] { 1, 1 }));
    }

    @Test
    public void testRemoveTuple() {
        ts.remove(new Integer[] { 0, 1 });
        assertFalse(ts.contains(new Integer[] { 0, 1 }));
        ts.remove(new Integer[] { 1, 1 });
    }

    @Test
    public void testIterator() {
        int i = 0;
        for (Iterator<Integer[]> itr = ts.iterator(); itr.hasNext();) {
            itr.next();
            i++;
        }
        assertEquals(i, ts.size());
    }

    @Test
    public void testSize() {
        assertEquals(3, ts.size());
        ts.remove(new Integer[] { 0, 0 });
        assertEquals(2, ts.size());
        ts.add(new Integer[] { 1, 1 });
        assertEquals(3, ts.size());
    }
}
