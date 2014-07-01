package cspom.util

import org.scalameter.api.Gen
import org.scalameter.api.PerformanceTest
import org.scalameter.persistence.SerializationPersistor
import org.scalameter.reporting.RegressionReporter
import org.scalameter.reporting.HtmlReporter
import org.scalameter.Reporter

object RangeMicrobenchmark extends PerformanceTest.OnlineRegressionReport {

  val sizes: Gen[Int] = Gen.range("size")(0, 200, 20)

  def simpleRanges = for (size <- sizes) yield {
    (IntRangeSet((0 until size).map(i => IntInterval.singleton(i * 13))),
      IntRangeSet((0 until size).map(i => IntInterval.singleton(i * 13 + 10000))))
  }

  def simpleRanges2 = for (size <- sizes) yield {
    (IntRangeSet((0 until size).map(i => IntInterval.singleton(i * 13))),
      IntRangeSet((0 until size).map(i => IntInterval.singleton(i * 13 + 130))))
  }

  performance of "RangeSet" in {
    measure method "union" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          r1 ++ r2
      }

      using(simpleRanges2) in {
        case (r1, r2) =>
          r1 ++ r2
      }
    }

    measure method "intersection" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          r1 & r2
      }

      using(simpleRanges2) in {
        case (r1, r2) =>
          r1 & r2
      }
    }

    measure method "difference" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          r1 -- r2
      }

      using(simpleRanges2) in {
        case (r1, r2) =>
          r1 -- r2
      }
    }
  }

  performance of "RangeArithmetics" in {
    measure method "plus" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          IntervalsArithmetic.RangeArithmetics(r1) + r2
      }
    }

    measure method "minus" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          IntervalsArithmetic.RangeArithmetics(r1) + r2
      }
    }

    measure method "times" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          IntervalsArithmetic.RangeArithmetics(r1) + r2
      }
    }

    measure method "div" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          IntervalsArithmetic.RangeArithmetics(r1) + r2
      }
    }

    measure method "neg" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          -IntervalsArithmetic.RangeArithmetics(r1)
      }
    }

    measure method "abs" in {
      using(simpleRanges) in {
        case (r1, r2) =>
          -IntervalsArithmetic.RangeArithmetics(r1)
      }
    }
  }
}