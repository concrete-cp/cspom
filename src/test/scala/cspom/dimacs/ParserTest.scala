package cspom.dimacs;


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class ParserTest extends AnyFlatSpec with Matchers {

  val FILENAME = "flat30-1.cnf";

  "CNFParser" should s"parse $FILENAME" in {
    val cspom = CNFParser(classOf[ParserTest].getResourceAsStream(FILENAME)).get

    cspom.expressionMap.count shouldBe 90
    cspom.constraints should have size 300

  }

}
