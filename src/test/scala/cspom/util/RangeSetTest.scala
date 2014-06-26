package cspom.util

import scala.collection.SortedSet

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

import Intervals.smallIntervals
import Intervals.validIntervals
import IntRangeSet._

class RangeSetTest extends FlatSpec with Matchers with PropertyChecks {

  implicit def asSet(r: IntRangeSet): SortedSet[Int] =
    new ContiguousIntRangeSet(r)

  "IntRangeSet" should "be conform" in {

    val rangeSet1: IntRangeSet = IntRangeSet()
    val rs2 = rangeSet1 ++ IntInterval(1, 10); // {[1, 10]}
    val rs3 = rs2 ++ IntInterval.closedOpen(11, 15); // disconnected range: {[1, 10], [11, 15)} 
    val rs4 = rs3 ++ IntInterval.closedOpen(15, 20); // connected range; {[1, 10], [11, 20)}
    val rs5 = rs4 ++ IntInterval.openClosed(0, 0); // empty range; {[1, 10], [11, 20)}
    val rs6 = rs5 -- IntInterval.open(5, 10); // splits [1, 10]; {[1, 5], [10, 10], [11, 20)}

    rangeSet1.toString shouldBe "{}"
    rs2.toString shouldBe "{[1‥10]}"
    rs3.toString shouldBe "{[1‥10], [11‥15)}"
    rs4.toString shouldBe "{[1‥10], [11‥20)}"
    rs5.toString shouldBe "{[1‥10], [11‥20)}"
    rs6.toString shouldBe "{[1‥5], [10‥10], [11‥20)}"
  }

  it should "contain all values when open" in {
    val i: IntInterval = IntInterval.all()

    forAll { j: Int => assert(i.contains(j)) }

    val l = IntInterval.atLeast(0)

    forAll { j: Int => l.contains(j) shouldBe j >= 0 }

  }

  it should "merge intersecting Ranges" in {
    val itv1 = IntInterval(0, 10)
    val itv2 = IntInterval(10, 15)

    IntRangeSet(Seq(itv1, itv2)) shouldBe 'convex

  }

  it should "work under disjoint IntRangeSet" in {

    IntRangeSet(Seq(
      IntInterval(0, 5),
      IntInterval(-10, -9),
      IntInterval(8, 10),
      IntInterval(-7, -4))) shouldBe IntRangeSet(Seq(
      IntInterval(-10, -9),
      IntInterval(-7, -4),
      IntInterval(0, 5),
      IntInterval(8, 10)))

  }

  it should "handle empty IntRangeSet" in {
    val itv1 = IntRangeSet()
    itv1 shouldBe empty

    itv1 ++ IntInterval(5, 10) shouldBe IntRangeSet(IntInterval(5, 10))
  }

  it should "handle large values" in {

    asSet(IntRangeSet() ++ List[IntInterval](0, 2, 1)) should contain theSameElementsInOrderAs
      Seq(0, 1, 2)

    asSet(IntRangeSet() ++ Int.MinValue ++ (Int.MaxValue - 1) ++ 0) should contain theSameElementsInOrderAs
      Seq(Int.MinValue, 0, Int.MaxValue - 1)

    val s = Set[IntInterval](0, -2147483648, 1, 2, -1, -2)

    (IntRangeSet() ++ s).ranges should contain theSameElementsAs s

  }

  it should "merge joint ranges" in {
    val itv1 = IntRangeSet(IntInterval.closedOpen(0, 6))

    (itv1 ++ IntInterval.closedOpen(-10, -8) ++ IntInterval(8, 10) ++ IntInterval(-8, -4)).ranges should contain theSameElementsAs Seq(
      IntInterval(-10, -4), IntInterval.closedOpen(0, 6), IntInterval(8, 10))

    itv1 ++ IntInterval(6, 10) shouldBe IntRangeSet(IntInterval(0, 10))

    itv1 ++ IntInterval.closedOpen(-5, 0) shouldBe IntRangeSet(IntInterval.closedOpen(-5, 6))
  }

  it should "compute difference" in {

    val i = IntRangeSet(Seq(IntInterval.singleton(2147483647), IntInterval.singleton(0))).canonical
    (i -- IntInterval.singleton(2147483647)).canonical shouldBe IntRangeSet(IntInterval.singleton(0)).canonical

    IntRangeSet(IntInterval(0, 5)) -- IntInterval(10, 15) shouldBe IntRangeSet(IntInterval(0, 5))
    IntRangeSet(IntInterval(10, 15)) -- IntInterval(0, 5) shouldBe IntRangeSet(IntInterval(10, 15))
    (IntRangeSet(IntInterval(0, 10)) -- IntInterval(3, 7)).canonical shouldBe
      IntRangeSet(Seq(IntInterval(0, 2), IntInterval(8, 10))).canonical
    (IntRangeSet(IntInterval(0, 10)) -- IntInterval(-10, 5)).canonical shouldBe
      IntRangeSet(IntInterval(6, 10)).canonical
    (IntRangeSet(IntInterval(0, 10)) -- IntInterval(5, 10)).canonical shouldBe
      IntRangeSet(IntInterval(0, 4)).canonical
    IntRangeSet(IntInterval(0, 5)) -- IntInterval(-5, 15) shouldBe IntRangeSet()
    IntRangeSet(IntInterval(0, 5)) -- IntInterval(0, 5) shouldBe IntRangeSet()

    forAll(smallIntervals, validIntervals) { (i1, i2) =>
      asSet(IntRangeSet(i1) -- i2) should contain theSameElementsInOrderAs
        i1.allValues.filterNot(i2.contains).toStream
    }

    forAll { (s1: Seq[Int], s2: Seq[Int]) =>
      val i1 = IntRangeSet(s1.map(IntInterval.singleton(_))).canonical
      val i2 = IntRangeSet(s2.map(IntInterval.singleton(_))).canonical

      asSet(i1 -- i2) should contain theSameElementsAs (s1.toSet -- s2.toSet)
    }

  }

  it should "compute differences of open intervals" in {
    IntRangeSet(Seq(IntInterval(0, 5), IntInterval(10, 15))) -- IntInterval.atMost(7) shouldBe
      IntRangeSet(IntInterval(10, 15))

    IntRangeSet(Seq(IntInterval(0, 5), IntInterval(10, 15))) -- IntInterval.atMost(-2) shouldBe
      IntRangeSet(Seq(IntInterval(0, 5), IntInterval(10, 15)))

    IntRangeSet(Seq(IntInterval(0, 5), IntInterval(10, 15))) -- IntInterval.atMost(15) shouldBe
      IntRangeSet()

    forAll { (s1: Seq[Int], b: Int) =>
      val s = IntRangeSet(s1.map(IntInterval.singleton(_))).canonical
      val d = s -- IntInterval.lessThan(b)

      asSet(d) should contain theSameElementsAs (s1.distinct.filter(_ >= b))
    }
  }

  it should "intersect" in {

    asSet(IntRangeSet(List(1, 2, 3).map(IntInterval.singleton(_))).canonical &
      IntRangeSet(List(2, 3, 4).map(IntInterval.singleton(_))).canonical) should contain theSameElementsAs
      Set(2, 3)

    forAll { (s1: Seq[Int], s2: Seq[Int]) =>
      val i1 = IntRangeSet(s1.map(IntInterval.singleton(_)))
      val i2 = IntRangeSet(s2.map(IntInterval.singleton(_)))

      asSet(i1 & i2) should contain theSameElementsAs (s1.toSet & s2.toSet)

    }

  }

  it should "allow infinite ranges" in {
    val all = IntRangeSet.all

    all.toString shouldBe "{(-∞‥+∞)}"

  }

  it should "detect equality" in {
    IntRangeSet(IntInterval.all) should not be IntRangeSet(IntInterval.greaterThan(0))
  }
  
  it should "contain given values" in {
    val r = IntRangeSet(Seq(IntInterval.singleton(0), IntInterval.singleton(-1)))
    val s = new ContiguousIntRangeSet(r)
    s should contain(0)
    s should contain(-1)
    
    Set(0, -1) shouldBe s
    s.iterator.toStream shouldBe Stream(-1, 0)
    
  }
} 