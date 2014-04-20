package cspom.extension;

import org.scalatest.Matchers
import org.scalatest.FlatSpec

final class MDDTest extends FlatSpec with Matchers {

  val relation: MDD[Int] = MDD.empty + Seq(2, 5, 5) + Seq(3, 5, 5)

  "Given MDD" should "conform to Set semantics" in {
    relation.asInstanceOf[Set[Seq[Int]]] should contain(Seq(2, 5, 5))
    relation.asInstanceOf[Set[Seq[Int]]] should not contain (Seq(1, 2, 3))
  }

  it should "have correct String representation" in {
    relation.tupleString should (be("2 5 5|3 5 5") or be("3 5 5|2 5 5"))
  }

  it should "detect egality" in {
    val r2: MDD[Int] = MDD.empty + Seq(3, 5, 5) + Seq(2, 5, 5)

    val r3: MDD[Int] = MDD.empty + Seq(2, 5, 5) + Seq(3, 5, 5)

    relation shouldBe r2
    relation shouldBe r3
    relation should not be (r2 + Seq(1, 2, 3))
  }

  it should "have correct size" in {
    relation should have size 2
  }

  it should "ignore already present tuples when adding" in {
    (relation + Seq(2, 5, 5)) should have size 2
  }

  it should "compute unions correctly" in {
    val r2 = MDD(Iterable(
      Seq(1, 2, 3), Seq(2, 5, 6), Seq(3, 5, 5)))

    r2.toSet ++ relation should ===(relation union r2)
  }

  it should "be filtered" in {
    relation.filter((depth, value) => depth != 1 || value != 5) shouldBe empty
    relation.filter((depth, value) => depth != 0 || value != 3) shouldBe Set(Seq(2, 5, 5))
  }

  it should "be projected" in {
    relation.project(Seq(1, 2)) should ===(Set(Seq(5, 5)))
  }
}
