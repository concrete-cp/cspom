package cspom;

import scala.util.Failure
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.TryValues
import scala.util.Try

object StatisticsManagerTest {
  @Statistic
  val testInt = 8;
  @Statistic
  val testLong = 9l;
  @Statistic
  val testFloat = 10.0f;
  @Statistic
  val testDouble = 11.0;
}

final class StatisticsManagerTest extends FlatSpec with Matchers with TryValues {

  "StatisticsManager" should "register objects" in {
    val stats = new StatisticsManager
    stats.register("test", StatisticsManagerTest);
    stats("test.testInt") shouldBe 8
    stats("test.testLong") shouldBe 9l
    stats("test.testFloat") shouldBe 10.0f
    stats("test.testDouble") shouldBe 11.0
  }

  it should "compute statistics" in {
    val seq = Seq(30, 53, 76, 77, 25, 66, 78, 50,
      70, 42, 90, 31, 79, 7, 39, 49, 15, 82, 35, 27)

    StatisticsManager.average(seq) shouldBe 51.05
    StatisticsManager.median(seq) shouldBe 50
    StatisticsManager.stDev(seq) shouldBe 24.81187 +- 0.00001

  }

  it should "measure time in regular case" in {
    val (r, t) = StatisticsManager.measure {
      Thread.sleep(1000)
    }

    t.value shouldBe 1000.0 +- 10
    r should be a 'success
  }

  it should "measure time in exception case" in {
    val (r, t) = StatisticsManager.measure {
      Thread.sleep(1000)
      throw new Exception
    }

    t.value shouldBe 1000.0 +- 10
    r should be a 'failure
  }

  it should "measure time in success case" in {
    val (r, t) = StatisticsManager.measureTry(Try(Thread.sleep(1000)))

    t.value shouldBe 1000.0 +- 10
    r should be a 'success
  }

  it should "measure time in failure case" in {
    val (r, t) = StatisticsManager.measureTry {
      Thread.sleep(1000)
      Failure(new Exception)
    }

    t.value shouldBe 1000.0 +- 10
    r should be a 'failure
  }

}
