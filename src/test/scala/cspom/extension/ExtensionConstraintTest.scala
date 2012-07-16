package cspom.extension;

import org.junit.Test
import org.junit.Before
import org.junit.Assert._
import org.hamcrest.CoreMatchers._
import org.junit.matchers.JUnitMatchers._

final class ExtensionConstraintTest {

  var relation: Trie = null;

  @Before
  def setUp() {
    relation = Trie.empty(3);
    relation += (2, 5, 5);
    relation += (3, 5, 5);
  }

  @Test
  def testEvaluate() {
    assertFalse(relation.contains(1, 2, 3));
    assertTrue(relation.contains(2, 5, 5));
  }

  @Test
  def testTuplesToString() {
    assertThat(relation.tupleString, either(equalTo("2 5 5|3 5 5")).or(equalTo("3 5 5|2 5 5")));
  }

  @Test
  def testAddTuple() {
    relation += (2, 5, 5);
    assertEquals(2, relation.size);
  }
}
