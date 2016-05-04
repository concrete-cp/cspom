package cspom.extension;

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.time.Seconds
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.Span

final class MDDTest extends FlatSpec with Matchers with Timeouts {

  val relation: MDD[Int] = MDD.empty + List(2, 5, 5) + List(3, 5, 5)

  "Given MDD" should "conform to Set semantics" in {
    relation.asInstanceOf[Set[Seq[Int]]] should contain(List(2, 5, 5))
    relation.asInstanceOf[Set[Seq[Int]]] should not contain (List(1, 2, 3))
  }

  it should "have correct String representation" in {
    relation.tupleString should (be("2 5 5|3 5 5") or be("3 5 5|2 5 5"))
  }

  it should "detect egality" in {
    val r2: MDD[Int] = MDD.empty + List(3, 5, 5) + List(2, 5, 5)

    val r3: MDD[Int] = MDD.empty + List(2, 5, 5) + List(3, 5, 5)

    relation shouldBe r2
    relation shouldBe r3
    relation should not be (r2 + List(1, 2, 3))
  }

  it should "have correct size" in {
    relation should have size 2

    (MDD.empty + List(138)).edges shouldBe 1
  }

  it should "ignore already present tuples when adding" in {
    (relation + List(2, 5, 5)) should have size 2
  }

  it should "compute unions correctly" in {
    val r2 = MDD(Iterable(
      List(1, 2, 3), List(2, 5, 6), List(3, 5, 5)))

    r2.toSet ++ relation should ===(relation union r2)
  }

  it should "be filtered" in {
    relation.filter((depth, value) => depth != 1 || value != 5) shouldBe empty
    relation.filter((depth, value) => depth != 0 || value != 3) shouldBe Set(List(2, 5, 5))
  }

  it should "be projected" in {
    relation.project(List(1, 2)) should ===(Set(List(5, 5)))
  }

  it should "be reduced" in {
    val r2 = relation.reduce
    r2 should have('edges(4))
    r2 shouldBe relation
    relation shouldBe r2
  }

  it should "be reduced quickly" in {
    var mdd: MDD[Int] = MDD.empty
    for (i <- 1 to 20; j <- 1 to 20; k <- 1 to 20; l <- 1 to 20) {
      mdd += List(i, j, k, l)
    }

    val r2 = failAfter(Span(2, Seconds)) {
      mdd.reduce
    }

    r2.edges shouldBe (80)
  }

  it should "add dimensions" in {

    val r2 = relation.insertDim(3, List(2, 4, 6))
    r2 should have('lambda(3 * relation.lambda))

  }

  it should "intersect" in {
    val r3 = MDD.empty + List(3, 5, 5) + List(1, 2, 2)
    val ri = relation.intersect(r3)
    ri should have('lambda(1))
    ri should have('edges(3))
  }

}
