package cspom.util

import org.scalameter.api._
import scala.collection.immutable.Stream.consWrapper

object RangeMicrobenchmark extends PerformanceTest.OfflineReport {

  val sizes: Gen[Int] = Gen.range("size")(0, 100, 5)

  def simpleRanges = for (size <- sizes) yield {
    (IntRangeSet((0 until size).map(i => IntInterval.singleton(i * 13))),
      IntRangeSet((0 until size).map(i => IntInterval.singleton(i * 13 + 10000))))
  }

  performance of "RangeArithmetics" in {
    measure method "+" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          IntervalsArithmetic.RangeArithmetics(r1) + r2
      }
    }
  }
}