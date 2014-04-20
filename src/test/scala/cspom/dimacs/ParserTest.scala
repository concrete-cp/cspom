package cspom.dimacs;

import cspom.CSPOM
import org.scalatest.Matchers
import org.scalatest.FlatSpec

final class ParserTest extends FlatSpec with Matchers {

  val FILENAME = "flat30-1.cnf";

  "CNFParser" should s"parse $FILENAME" in {
    val cspom = CNFParser.parse(classOf[ParserTest].getResourceAsStream(FILENAME))._1
    cspom.namedExpressions should have size 90
    cspom.constraints should have size 300
  }

}
