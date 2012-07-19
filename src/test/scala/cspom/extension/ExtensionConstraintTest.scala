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
    relation = Trie.empty;
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
  def testSize() {
    assertEquals(2, relation.size)
    assertEquals(3, relation.depth)
  }

  @Test
  def testAddTuple() {
    relation += (2, 5, 5);
    assertEquals(2, relation.size);
  }

  @Test
  def testRemove() {
    relation -= Array(2, 5, 5)
    assertEquals(1, relation.size)
    assertEquals("3 5 5", relation.tupleString)
    relation -= Array(3, 5, 5)
    assertEquals(0, relation.size)
    assertEquals("", relation.tupleString)
  }

  @Test
  def testFilter() {
    assertSame(Trie.empty, relation.filterTrie({
      case (depth, value) => depth != 1 || value != 5
    }))
    assertEquals("2 5 5", relation.filterTrie({
      case (depth, value) => depth != 0 || value != 3
    }).tupleString)
  }
}
