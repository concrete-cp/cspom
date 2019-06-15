package cspom

import cspom.variable.{CSPOMSeq, IntVariable}
import fastparse.Parsed
import org.scalatest.{FlatSpec, Matchers}

class NameParserTest extends FlatSpec with Matchers {

  "NameParser" should "parse array" in {
    val Parsed.Success((name, indices), _) = fastparse.parse("pdr[0]", NameParser(_))

    //    assert(parsed.isSuccess)
    //
    //    val (name, indices) = parsed.get.value

    name shouldBe "pdr"
    indices shouldBe Seq(0)



  }

  it should "parse id" in {
    val Parsed.Success((name, indices), _) = fastparse.parse("pdr", NameParser(_))
    name shouldBe "pdr"
    indices shouldBe Nil
  }

  it should "parse ids with underscores" in {
    val Parsed.Success((name, indices), _) = fastparse.parse("_10", NameParser(_))
    name shouldBe "_10"
    indices shouldBe Nil
  }

  "ExpressionMap" should "find array element" in {
    val cspom = CSPOM { implicit problem =>
      val vars = Seq.fill(10)(IntVariable(0 to 10))

      CSPOMSeq(vars: _*) as "sequence"
    }

    assert(cspom.expression("sequence[0]").isDefined)
  }

}
