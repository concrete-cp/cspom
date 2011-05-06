package cspom.extension;

import org.junit.Test
import org.junit.Before
import org.junit.Assert._

final class ExtensionConstraintTest {

    var relation:Relation = null;

    @Before
    def setUp() {
        relation = new Relation(3);
        relation.addTuple(2, 5, 5);
        relation.addTuple(3, 5, 5);
    }

    @Test
    def testEvaluate() {
        assertFalse(relation.containsTuple(1, 2, 3));
        assertTrue(relation.containsTuple(2, 5, 5));
    }

    @Test
    def testTuplesToString() {
    	val desc = relation.tupleString
        assertTrue(desc == "2 5 5|3 5 5" || desc == "3 5 5|2 5 5");
    }

    @Test
    def testAddTuple() {
        relation.add(Array(2, 5, 5));
        assertEquals(2, relation.size);
    }
}
