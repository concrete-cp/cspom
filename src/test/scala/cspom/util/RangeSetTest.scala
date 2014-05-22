package cspom.util

import org.scalatest.Matchers
import com.google.common.collect.TreeRangeSet
import org.scalatest.FlatSpec
import com.google.common.collect.Range

class RangeSetTest extends FlatSpec with Matchers {

  "RangeSet" should "be conform" in {
    val rangeSet1: RangeSet[Int] = RangeSet.empty
    rangeSet1 + Range.closed(1, 10); // {[1, 10]}
    rangeSet.add(Range.closedOpen(11, 15)); // disconnected range: {[1, 10], [11, 15)} 
    rangeSet.add(Range.closedOpen(15, 20)); // connected range; {[1, 10], [11, 20)}
    rangeSet.add(Range.openClosed(0, 0)); // empty range; {[1, 10], [11, 20)}
    rangeSet.remove(Range.open(5, 10)); // splits [1, 10]; {[1, 5], [10, 10], [11, 20)}
  }
} 