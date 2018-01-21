package cspom.xcsp

import cspom.extension.MDDRelation
import cspom.{CSPOM, CSPOMConstraint}
import org.scalatest.{FlatSpec, Inspectors, Matchers}

final class ParserTest extends FlatSpec with Matchers with Inspectors {
  for (file <- Seq("ModelFile.xml.xz", "testExtension1.xml.xz", "testExtension2.xml.xz", "testPrimitive.xml.xz")) {

    it should s"parse XCSP3 $file" in {
      val cspom = CSPOM.load(classOf[ParserTest].getResource(file)).get

      cspom.expressionMap.expressionsWithNames.size should be >= 1
      cspom.constraints.size should be >= 1
    }
  }

  "XCSP parser" should "correctly parse XCSP3 MDD" in {
    val cspom = CSPOM.load(classOf[ParserTest].getResource("MagicSquare-3-mdd.xml.xz")).get

    cspom.expressionMap.expressionsWithNames.size should be >= 1
    cspom.constraints should have size 9


    val mdds = cspom.constraints.collect { case c: CSPOMConstraint[_] if c.function == 'extension => c.getParam[MDDRelation]("relation").get }.toSeq
    mdds should have size 8
    forAll(mdds) { m: MDDRelation =>
      m.mdd.vertices() shouldBe 20
      m.mdd.edges() shouldBe 79
      m.arity shouldBe 3
    }

  }

  it should "correctly parse MDD" in {
    val relation = """
0 1 |0 2 |0 3 |0 4 |1 0 |1 2 |1 3 |1 4 |2 0 |2 1 |2 3 |2 4 |3 0 |3 1 |3 2 |3 4 |4 0 |4 1 |4 2 |4 3 |
                   """

    val mdd = ConstraintParser.parseTable(relation, 2, 20)

    mdd.lambda shouldBe 20
  }

}
