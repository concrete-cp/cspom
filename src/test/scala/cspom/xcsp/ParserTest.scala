package cspom.xcsp;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import cspom.CSPOM
import cspom.compiler.CSPOMCompiler
import scala.util.Failure
import scala.util.Success
import org.scalatest.exceptions.TestFailedException

final class ParserTest extends FlatSpec with Matchers {
  val FILENAME = "crossword-m1-debug-05-01.xml";

  "XCSPParser" should s"parse $FILENAME" in {

    XCSPParser(classOf[ParserTest].getResourceAsStream(FILENAME)) match {
      case Success((cspom, _)) =>
        cspom.expressionsWithNames should have size 25
        assert(!cspom.expressionsWithNames.exists(e => cspom.getAnnotations(e._1).hasParam("var_is_introduced")))

        cspom.constraints.size should be >= 55

      case Failure(e) => fail(e)//throw new TestFailedException(e)
    }
    //println(cspom);

  }
}
