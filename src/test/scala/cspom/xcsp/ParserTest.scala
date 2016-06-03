package cspom.xcsp;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import cspom.CSPOM
import cspom.compiler.CSPOMCompiler
import scala.util.Failure
import scala.util.Success
import org.scalatest.exceptions.TestFailedException

final class ParserTest extends FlatSpec with Matchers {
  val FILENAME = "crossword-m1-debug-05-01.xml.lzma";

  "XCSPParser" should s"parse XCSP2 $FILENAME" in {

    CSPOM.load(classOf[ParserTest].getResource(FILENAME))
      .map { cspom =>
        cspom.expressionsWithNames should have size 25
        cspom.constraints.size should be >= 55
      }.recover {
        case e => fail(e) //throw new TestFailedException(e)
      }
    //println(cspom);

  }

  for (file <- Seq("testExtension1.xml.xz", "testExtension2.xml.xz", "testPrimitive.xml.xz")) {

    it should s"parse XCSP3 $file" in {
      CSPOM.load(classOf[ParserTest].getResource(file))
        .map { cspom =>
          cspom.expressionsWithNames.size should be >= 1
          cspom.constraints.size should be >= 1
        }.get
    }
  }
}
