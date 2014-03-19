package cspom.extension

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

final class HashTrieTest {

  private var ts: MDD[Int] = null

  @Before
  def setUp() {
    ts = MDD.empty;
    ts += (0, 0)
    ts += (0, 1)
    ts += (1, 0)
  }

  @Test
  def testContainsTuple() {
    assertTrue(ts.contains(Seq(0, 1)));
    assertFalse(ts.contains(Seq(1, 1)));
  }

  @Test
  def testIterator() {
    var i = 0;
    val itr = ts.iterator
    while (itr.hasNext) {
      itr.next();
      i += 1;
    }
    assertEquals(i, ts.size)
  }

  @Test
  def testSize() {
    assertEquals(3, ts.size);
    ts += (1, 1);
    assertEquals(4, ts.size);
  }

  @Test
  def testTrie() {
    val t = MDD.empty + (1, 2, 3) + (1, 3, 4) + (1, 2, 5) + (2, 3, 5)
    assertEquals(4, t.size)

    assertTrue(t.contains(Array(1, 3, 4)))
    assertFalse(t.contains(Array(1, 2, 4)))

    var s = MDD.empty + (1, 2, 5) + (1, 3, 4) + (1, 2, 3)

    assertFalse(t == s)

    s += (2, 3, 5)

    assertEquals(t.toSet, s.toSet)

    var u = MDD(Iterable(
      Seq(1, 2, 3),
      Seq(1, 3, 4),
      Seq(1, 2, 5),
      Seq(2, 3, 5)))

    assertEquals(t.toSet, u.toSet)

  }
}
