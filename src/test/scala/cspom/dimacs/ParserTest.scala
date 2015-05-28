package cspom.dimacs;

import cspom.CSPOM
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.util.Failure
import scala.util.Success

final class ParserTest extends FlatSpec with Matchers {

  val FILENAME = "flat30-1.cnf";

  "CNFParser" should s"parse $FILENAME" in {
    CNFParser(classOf[ParserTest].getResourceAsStream(FILENAME)) match {
      case Success((cspom, _)) =>
        cspom.expressionsWithNames should have size 90
        cspom.constraints should have size 300
      case Failure(e) => fail(e)
    }

  }

}
