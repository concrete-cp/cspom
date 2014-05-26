package cspom.util

import org.scalatest.Matchers
import com.google.common.collect.TreeRangeSet
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import cspom.variable.IntDiscreteDomain

class RangeSetTest extends FlatSpec with Matchers with PropertyChecks {

  "RangeSet" should "be conform" in {
    val rangeSet1: RangeSet[Int] = RangeSet.empty
    val rs2 = rangeSet1 + GuavaRange.closed(1, 10); // {[1, 10]}
    val rs3 = rs2 + GuavaRange.closedOpen(11, 15); // disconnected range: {[1, 10], [11, 15)} 
    val rs4 = rs3 + GuavaRange.closedOpen(15, 20); // connected range; {[1, 10], [11, 20)}
    val rs5 = rs4 + GuavaRange.openClosed(0, 0); // empty range; {[1, 10], [11, 20)}
    val rs6 = rs5 - GuavaRange.open(5, 10); // splits [1, 10]; {[1, 5], [10, 10], [11, 20)}

    rangeSet1.toString shouldBe "{}"
    rs2.toString shouldBe "{[1‥10]}"
    rs3.toString shouldBe "{[1‥10], [11‥15)}"
    rs4.toString shouldBe "{[1‥10], [11‥20)}"
    rs5.toString shouldBe "{[1‥10], [11‥20)}"
    rs6.toString shouldBe "{[1‥5], [10‥10], [11‥20)}"
  }

  it should "contain all values when open" in {
    val i: GuavaRange[Int] = GuavaRange.all()

    forAll { j: Int => assert(i.contains(j)) }

    val l = GuavaRange.atLeast(0)

    forAll { j: Int => l.contains(j) shouldBe j >= 0 }

  }

  it should "merge intersecting Ranges" in {
    val itv1 = GuavaRange.closed(0, 10)
    val itv2 = GuavaRange.closed(10, 15)

    RangeSet(itv1, itv2) shouldBe 'convex

  }
  //
  //  it should "accept correct trees" in {
  //    noException should be thrownBy new RangeSet(GuavaRange.closed(0, 5), None, None)
  //    noException should be thrownBy RangeSet(GuavaRange.closed(5, 0), None, None)
  //    noException should be thrownBy RangeSet(GuavaRange.closed(0, 5), Some(RangeSet(-5, -1)), None)
  //  }
  //
  //  it should "reject incorrect trees" in {
  //    an[IllegalArgumentException] should be thrownBy RangeSet(GuavaRange.closed(5, 0), Some(RangeSet(0, 5)), None)
  //    an[IllegalArgumentException] should be thrownBy RangeSet(GuavaRange.closed(0, 5), Some(RangeSet(5, 0)), None)
  //    an[IllegalArgumentException] should be thrownBy RangeSet(GuavaRange.closed(0, 5), Some(RangeSet(-5, 0)), None)
  //  }

  it should "work under disjoint RangeSet" in {

    RangeSet(
      GuavaRange.closed(0, 5),
      GuavaRange.closed(-10, -9),
      GuavaRange.closed(8, 10),
      GuavaRange.closed(-7, -4)) shouldBe RangeSet(
        GuavaRange.closed(-10, -9),
        GuavaRange.closed(-7, -4),
        GuavaRange.closed(0, 5),
        GuavaRange.closed(8, 10))

  }

  it should "handle empty RangeSet" in {
    val itv1 = RangeSet[Int]()
    itv1 shouldBe empty

    itv1 + GuavaRange.closed(5, 10) shouldBe RangeSet(GuavaRange.closed(5, 10))
  }

  it should "handle large values" in {

    implicit def single(i: Int) = GuavaRange.ofInt(i)

    IntDiscreteDomain.allValues(
      RangeSet[Int]() ++ List(0, 2, 1).map(single)) should contain theSameElementsAs Set(0, 1, 2)

    IntDiscreteDomain.allValues(
      RangeSet[Int]() + Int.MinValue + (Int.MaxValue - 1) + 0) should contain theSameElementsAs
      Set(Int.MinValue, 0, Int.MaxValue - 1)

    val s = Set[GuavaRange[Int]](0, -2147483648, 1, 2, -1, -2)

    (RangeSet[Int]() ++ s).ranges should contain theSameElementsAs s

  }

  it should "merge joint ranges" in {
    val itv1 = RangeSet(GuavaRange.closedOpen(0, 6))

    (itv1 + GuavaRange.closedOpen(-10, -8) + GuavaRange.closed(8, 10) + GuavaRange.closed(-8, -4)).ranges shouldBe Seq(
      GuavaRange.closed(-10, -4), GuavaRange.closedOpen(0, 6), GuavaRange.closed(8, 10))

    itv1 + GuavaRange.closed(6, 10) shouldBe RangeSet(GuavaRange.closed(0, 10))

    itv1 + GuavaRange.closedOpen(-5, 0) shouldBe RangeSet(GuavaRange.closedOpen(-5, 6))
  }

  it should "compute difference" in {
    import IntervalTest.Interval

    RangeSet(Interval(0, 5)) - Interval(10, 15) shouldBe RangeSet(Interval(0, 5))
    RangeSet(Interval(10, 15)) - Interval(0, 5) shouldBe RangeSet(Interval(10, 15))
    RangeSet(Interval(0, 10)) - Interval(3, 7) shouldBe RangeSet(Interval(0, 2), Interval(8, 10))
    RangeSet(Interval(0, 10)) - Interval(-10, 5) shouldBe RangeSet(Interval(6, 10))
    RangeSet(Interval(0, 10)) - Interval(5, 10) shouldBe RangeSet(Interval(0, 4))
    RangeSet(Interval(0, 5)) - Interval(-5, 15) shouldBe RangeSet[Int]()
    RangeSet(Interval(0, 5)) - Interval(0, 5) shouldBe RangeSet[Int]()

    import IntervalTest._

    forAll(smallIntervals, validIntervals) { (i1, i2) =>
      IntDiscreteDomain.allValues(RangeSet(i1) - i2).shouldBe(
        IntDiscreteDomain.allValues(i1).filterNot(i2.contains))
    }
  }
  //
  //  it should "merge overlapping RangeSet" in {
  //    val itv1 = RangeSet(0, 5) + GuavaRange.closed(-10, -9) + GuavaRange.closed(8, 10)
  //
  //    (itv1 + GuavaRange.closed(-10, -4)).RangeSet shouldBe Seq(
  //      GuavaRange.closed(-10, -4), GuavaRange.closed(0, 5), GuavaRange.closed(8, 10))
  //
  //    (itv1 + GuavaRange.closed(-11, -4)).RangeSet shouldBe Seq(
  //      GuavaRange.closed(-11, -4), GuavaRange.closed(0, 5), GuavaRange.closed(8, 10))
  //
  //    (itv1 + GuavaRange.closed(-11, 0)).RangeSet shouldBe Seq(
  //      GuavaRange.closed(-11, 5), GuavaRange.closed(8, 10))
  //
  //    (itv1 + GuavaRange.closed(-11, 39)).RangeSet shouldBe Seq(
  //      GuavaRange.closed(-11, 39))
  //  }
  //
  //  it should "behave like a SortedSet" in {
  //
  //    forAll { s: Seq[Int] =>
  //      RangeSet() ++ s shouldBe s.toSet
  //    }
  //  }
  //
  //  it should "compute differences" in {
  //    val s1 = List(0, -1, -2147483648)
  //    val s2 = List(0, -2147483648)
  //
  //    val i1 = RangeSet() ++ s1
  //    val i2 = RangeSet() ++ s2
  //    (i1 -- i2) shouldBe Set(-1)
  //
  //    forAll { (s1: Seq[Int], s2: Seq[Int]) =>
  //      val i1: RangeSet = RangeSet() ++ s1
  //      val i2 = RangeSet() ++ s2
  //
  //      (i1 -- i2) shouldBe (s1.toSet -- s2.toSet)
  //    }
  //  }
  //
  //  it should "intersect" in {
  //    forAll { (s1: Seq[Int], s2: Seq[Int]) =>
  //      val i1: RangeSet = RangeSet() ++ s1
  //      val i2 = RangeSet() ++ s2
  //
  //      (i1 & i2) shouldBe (s1.toSet & s2.toSet)
  //
  //    }
  //  }
} 