package cspom.variable

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import scala.collection.SortedSet

class IntervalsTest extends FlatSpec with Matchers with PropertyChecks {

  "Intervals" should "merge intersecting intervals" in {
    val itv1 = Intervals(0, 10)
    val itv2 = Interval(10, 15)

    (itv1 + itv2).lower shouldBe empty
    (itv1 + itv2).upper shouldBe empty
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

    (itv1 + Interval(-10, -9) + Interval(8, 10) + Interval(-7, -4)).asSequence shouldBe Seq(
      Interval(-10, -9), Interval(-7, -4), Interval(0, 5), Interval(8, 10))

  }

  it should "handle empty intervals" in {
    val itv1 = Intervals()
    itv1 shouldBe empty
    itv1.lower shouldBe empty
    itv1.upper shouldBe empty

    itv1 + Interval(5, 10) shouldBe Intervals(5, 10)
  }

  it should "merge joint intervals" in {
    val itv1 = Intervals(0, 5)

    (itv1 + Interval(-10, -9) + Interval(8, 10) + Interval(-8, -4)).asSequence shouldBe Seq(
      Interval(-10, -4), Interval(0, 5), Interval(8, 10))

    (itv1 + Interval(6, 10)) shouldBe Intervals(0, 10)

    (itv1 + Interval(-5, -1)) shouldBe Intervals(-5, 5)
  }

  it should "merge overlapping intervals" in {
    val itv1 = Intervals(0, 5) + Interval(-10, -9) + Interval(8, 10)

    (itv1 + Interval(-10, -4)).asSequence shouldBe Seq(
      Interval(-10, -4), Interval(0, 5), Interval(8, 10))

    (itv1 + Interval(-11, -4)).asSequence shouldBe Seq(
      Interval(-11, -4), Interval(0, 5), Interval(8, 10))

    (itv1 + Interval(-11, 0)).asSequence shouldBe Seq(
      Interval(-11, 5), Interval(8, 10))

    (itv1 + Interval(-11, 39)).asSequence shouldBe Seq(
      Interval(-11, 39))
  }

  it should "behave like a SortedSet" in {
    Intervals() + Int.MinValue + Int.MaxValue shouldBe Set(Int.MinValue, Int.MaxValue)

    forAll { s: SortedSet[Int] =>
      val itv = s.foldLeft(Intervals())(_ + _)
      itv shouldBe s
    }
  }

}