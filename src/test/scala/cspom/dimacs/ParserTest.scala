package cspom.dimacs;


import org.scalatest.Matchers
import org.scalatest.FlatSpec

final class ParserTest extends FlatSpec with Matchers {

  val FILENAME = "flat30-1.cnf";

  "CNFParser" should s"parse $FILENAME" in {
    val cspom = CNFParser(classOf[ParserTest].getResourceAsStream(FILENAME)).get

    cspom.expressionMap.count shouldBe 90
    cspom.constraints should have size 300

  }

}
