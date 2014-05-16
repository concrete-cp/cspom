package cspom.variable

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import scala.collection.SortedSet

class IntervalsTest extends FlatSpec with Matchers with PropertyChecks {

  "Intervals" should "merge intersecting intervals" in {
    val itv1 = Intervals(0, 10)
    val itv2 = Interval(10, 15)

    (itv1 + itv2) shouldBe 'convex

  }
  //
  //  it should "accept correct trees" in {
  //    noException should be thrownBy new Intervals(Interval(0, 5), None, None)
  //    noException should be thrownBy Intervals(Interval(5, 0), None, None)
  //    noException should be thrownBy Intervals(Interval(0, 5), Some(Intervals(-5, -1)), None)
  //  }
  //
  //  it should "reject incorrect trees" in {
  //    an[IllegalArgumentException] should be thrownBy Intervals(Interval(5, 0), Some(Intervals(0, 5)), None)
  //    an[IllegalArgumentException] should be thrownBy Intervals(Interval(0, 5), Some(Intervals(5, 0)), None)
  //    an[IllegalArgumentException] should be thrownBy Intervals(Interval(0, 5), Some(Intervals(-5, 0)), None)
  //  }

  it should "work under disjoint intervals" in {

    val itv1 = Intervals(0, 5)

    (itv1 + Interval(-10, -9) + Interval(8, 10) + Interval(-7, -4)).intervals shouldBe Seq(
      Interval(-10, -9), Interval(-7, -4), Interval(0, 5), Interval(8, 10))

  }

  it should "handle empty intervals" in {
    val itv1 = Intervals()
    itv1 shouldBe empty

    itv1 + Interval(5, 10) shouldBe Intervals(5, 10)
  }

  it should "handle large values" in {

    Intervals() ++ List(0, 2, 1) shouldBe Set(0, 1, 2)

    Intervals() + Int.MinValue + Int.MaxValue + 0 shouldBe Set(Int.MinValue, 0, Int.MaxValue)

    val s = Set(0, -2147483648, 1, 2, -1, -2)
    Intervals() ++ s shouldBe s

  }

  it should "merge joint intervals" in {
    val itv1 = Intervals(0, 5)

    (itv1 + Interval(-10, -9) + Interval(8, 10) + Interval(-8, -4)).intervals shouldBe Seq(
      Interval(-10, -4), Interval(0, 5), Interval(8, 10))

    (itv1 + Interval(6, 10)) shouldBe Intervals(0, 10)

    (itv1 + Interval(-5, -1)) shouldBe Intervals(-5, 5)
  }

  it should "merge overlapping intervals" in {
    val itv1 = Intervals(0, 5) + Interval(-10, -9) + Interval(8, 10)

    (itv1 + Interval(-10, -4)).intervals shouldBe Seq(
      Interval(-10, -4), Interval(0, 5), Interval(8, 10))

    (itv1 + Interval(-11, -4)).intervals shouldBe Seq(
      Interval(-11, -4), Interval(0, 5), Interval(8, 10))

    (itv1 + Interval(-11, 0)).intervals shouldBe Seq(
      Interval(-11, 5), Interval(8, 10))

    (itv1 + Interval(-11, 39)).intervals shouldBe Seq(
      Interval(-11, 39))
  }

  it should "behave like a SortedSet" in {

    forAll { s: Seq[Int] =>
      Intervals() ++ s shouldBe s.toSet
    }
  }

  it should "compute differences" in {
    val s1 = List(0, -1, -2147483648)
    val s2 = List(0, -2147483648)

    val i1 = Intervals() ++ s1
    val i2 = Intervals() ++ s2
    (i1 -- i2) shouldBe Set(-1)

    forAll { (s1: Seq[Int], s2: Seq[Int]) =>
      val i1: Intervals = Intervals() ++ s1
      val i2 = Intervals() ++ s2

      (i1 -- i2) shouldBe (s1.toSet -- s2.toSet)
    }
  }

  it should "intersect" in {
    forAll { (s1: Seq[Int], s2: Seq[Int]) =>
      val i1: Intervals = Intervals() ++ s1
      val i2 = Intervals() ++ s2

      (i1 & i2) shouldBe (s1.toSet & s2.toSet)

    }
  }

}