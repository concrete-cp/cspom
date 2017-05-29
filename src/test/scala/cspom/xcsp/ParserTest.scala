package cspom.xcsp

import cspom.extension.MDDRelation
import cspom.{CSPOM, CSPOMConstraint}
import org.scalatest.{FlatSpec, Inspectors, Matchers}

final class ParserTest extends FlatSpec with Matchers with Inspectors {
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
      val cspom = CSPOM.load(classOf[ParserTest].getResource(file)).get

      cspom.expressionsWithNames.size should be >= 1
      cspom.constraints.size should be >= 1
    }
  }

  it should "correctly parse XCSP3 MDD" in {
    val cspom = CSPOM.load(classOf[ParserTest].getResource("MagicSquare-3-mdd.xml.xz")).get

    cspom.expressionsWithNames.size should be >= 1
    cspom.constraints should have size 9


    val mdds = cspom.constraints.collect { case c: CSPOMConstraint[_] if c.function == 'extension => c.getParam[MDDRelation]("relation").get }.toSeq
    mdds should have size 8
    forAll(mdds) { m: MDDRelation =>
      m.mdd.vertices() shouldBe 20
      m.mdd.edges() shouldBe 79
      m.arity shouldBe 3
    }

  }

}
