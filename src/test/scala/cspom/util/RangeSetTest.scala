package cspom.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import Intervals.smallIntervals
import Intervals.validIntervals
import scala.collection.SortedSet
import RangeSet._

class RangeSetTest extends FlatSpec with Matchers with PropertyChecks {

  implicit def asSet(r: RangeSet[Int]): SortedSet[Int] =
    new ContiguousRangeSet(r, IntDiscreteDomain)

  implicit def dt = IntDiscreteDomain

  "RangeSet" should "be conform" in {

    val rangeSet1: RangeSet[Int] = RangeSet()
    val rs2 = rangeSet1 ++ Interval(1, 10); // {[1, 10]}
    val rs3 = rs2 ++ Interval.closedOpen(11, 15); // disconnected range: {[1, 10], [11, 15)} 
    val rs4 = rs3 ++ Interval.closedOpen(15, 20); // connected range; {[1, 10], [11, 20)}
    val rs5 = rs4 ++ Interval.openClosed(0, 0); // empty range; {[1, 10], [11, 20)}
    val rs6 = rs5 -- Interval.open(5, 10); // splits [1, 10]; {[1, 5], [10, 10], [11, 20)}

    rangeSet1.toString shouldBe "{}"
    rs2.toString shouldBe "{[1‥10]}"
    rs3.toString shouldBe "{[1‥10], [11‥15)}"
    rs4.toString shouldBe "{[1‥10], [11‥20)}"
    rs5.toString shouldBe "{[1‥10], [11‥20)}"
    rs6.toString shouldBe "{[1‥5], [10‥10], [11‥20)}"
  }

  it should "contain all values when open" in {
    val i: Interval[Int] = Interval.all()

    forAll { j: Int => assert(i.contains(j)) }

    val l = Interval.atLeast(0)

    forAll { j: Int => l.contains(j) shouldBe j >= 0 }

  }

  it should "merge intersecting Ranges" in {
    val itv1 = Interval(0, 10)
    val itv2 = Interval(10, 15)

    RangeSet(Seq(itv1, itv2)) shouldBe 'convex

  }

  it should "work under disjoint RangeSet" in {

    RangeSet(Seq(
      Interval(0, 5),
      Interval(-10, -9),
      Interval(8, 10),
      Interval(-7, -4))) shouldBe RangeSet(Seq(
      Interval(-10, -9),
      Interval(-7, -4),
      Interval(0, 5),
      Interval(8, 10)))

  }

  it should "handle empty RangeSet" in {
    val itv1 = RangeSet[Int]()
    itv1 shouldBe empty

    itv1 ++ Interval(5, 10) shouldBe RangeSet(Interval(5, 10))
  }

  it should "handle large values" in {

    asSet(RangeSet[Int]() ++ List[Interval[Int]](0, 2, 1)) should contain theSameElementsInOrderAs
      Seq(0, 1, 2)

    asSet(RangeSet[Int]() ++ Int.MinValue ++ (Int.MaxValue - 1) ++ 0) should contain theSameElementsInOrderAs
      Seq(Int.MinValue, 0, Int.MaxValue - 1)

    val s = Set[Interval[Int]](0, -2147483648, 1, 2, -1, -2)

    (RangeSet[Int]() ++ s).ranges should contain theSameElementsAs s

  }

  it should "merge joint ranges" in {
    val itv1 = RangeSet(Interval.closedOpen(0, 6))

    (itv1 ++ Interval.closedOpen(-10, -8) ++ Interval(8, 10) ++ Interval(-8, -4)).ranges should contain theSameElementsAs Seq(
      Interval(-10, -4), Interval.closedOpen(0, 6), Interval(8, 10))

    itv1 ++ Interval(6, 10) shouldBe RangeSet(Interval(0, 10))

    itv1 ++ Interval.closedOpen(-5, 0) shouldBe RangeSet(Interval.closedOpen(-5, 6))
  }

  it should "compute difference" in {

    val i = RangeSet(Seq(Interval.singleton(2147483647), Interval.singleton(0))).canonical
    (i -- Interval.singleton(2147483647)).canonical shouldBe RangeSet(Interval.singleton(0)).canonical

    RangeSet(Interval(0, 5)) -- Interval(10, 15) shouldBe RangeSet(Interval(0, 5))
    RangeSet(Interval(10, 15)) -- Interval(0, 5) shouldBe RangeSet(Interval(10, 15))
    (RangeSet(Interval(0, 10)) -- Interval(3, 7)).canonical shouldBe
      RangeSet(Seq(Interval(0, 2), Interval(8, 10))).canonical
    (RangeSet(Interval(0, 10)) -- Interval(-10, 5)).canonical shouldBe
      RangeSet(Interval(6, 10)).canonical
    (RangeSet(Interval(0, 10)) -- Interval(5, 10)).canonical shouldBe
      RangeSet(Interval(0, 4)).canonical
    RangeSet(Interval(0, 5)) -- Interval(-5, 15) shouldBe RangeSet[Int]()
    RangeSet(Interval(0, 5)) -- Interval(0, 5) shouldBe RangeSet[Int]()

    forAll(smallIntervals, validIntervals) { (i1, i2) =>
      asSet(RangeSet(i1) -- i2) should contain theSameElementsInOrderAs
        i1.allValues.filterNot(i2.contains).toStream
    }

    forAll { (s1: Seq[Int], s2: Seq[Int]) =>
      val i1 = RangeSet(s1.map(Interval.singleton(_))).canonical
      val i2 = RangeSet(s2.map(Interval.singleton(_))).canonical

      asSet(i1 -- i2) should contain theSameElementsAs (s1.toSet -- s2.toSet)
    }

  }

  it should "compute differences of open intervals" in {
    RangeSet(Seq(Interval(0, 5), Interval(10, 15))) -- Interval.atMost(7) shouldBe
      RangeSet(Interval(10, 15))

    RangeSet(Seq(Interval(0, 5), Interval(10, 15))) -- Interval.atMost(-2) shouldBe
      RangeSet(Seq(Interval(0, 5), Interval(10, 15)))

    RangeSet(Seq(Interval(0, 5), Interval(10, 15))) -- Interval.atMost(15) shouldBe
      RangeSet[Int]()

    forAll { (s1: Seq[Int], b: Int) =>
      val s = RangeSet(s1.map(Interval.singleton(_))).canonical
      val d = s -- Interval.lessThan(b)

      asSet(d) should contain theSameElementsAs (s1.distinct.filter(_ >= b))
    }
  }

  it should "intersect" in {

    implicit def dt = IntDiscreteDomain

    asSet(RangeSet(List(1, 2, 3).map(Interval.singleton(_))).canonical &
      RangeSet(List(2, 3, 4).map(Interval.singleton(_))).canonical) should contain theSameElementsAs
      Set(2, 3)

    forAll { (s1: Seq[Int], s2: Seq[Int]) =>
      val i1 = RangeSet(s1.map(Interval.singleton(_)))
      val i2 = RangeSet(s2.map(Interval.singleton(_)))

      asSet(i1 & i2) should contain theSameElementsAs (s1.toSet & s2.toSet)

    }

  }

  it should "allow infinite ranges" in {
    val all = RangeSet.all[Int]

    all.toString shouldBe "{(-∞‥+∞)}"

  }

  it should "detect equality" in {
    RangeSet(Interval.all[Int]) should not be RangeSet(Interval.greaterThan(0))
  }
} 