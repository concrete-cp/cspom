package cspom.flatzinc

import cspom.CSPOM
import cspom.compiler.{CSPOMCompiler, StandardCompilers}
import cspom.variable.CSPOMConstant
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlatZincTest extends AnyFlatSpec with Matchers {

  val url = classOf[FlatZincTest].getResource("1d_rubiks_cube.fzn.xz")

  "FlatZinc parser" should s"parse $url" in {
    val cspom = CSPOM.load(url, FlatZincFastParser).get

    // CSPOMCompiler.compile(cspom, FZPatterns()).get

    //val Seq(fz) = FZPatterns()

    cspom.constraints should not be empty

    // val constraint = cspom.constraints.next
    //    val delta = for (data <- fz.mtch(constraint, cspom)) yield {
    //      val delta = fz.compile(constraint, cspom, data)
    //      //println(delta.toString(vn))
    //    }

    //println(cspom)

  }

  it should "correctly affect values" in {
    val cspom = CSPOM.load(
      classOf[FlatZincTest].getResource("affectations.fzn.xz"), FlatZincFastParser).get
    CSPOMCompiler.compile(cspom, StandardCompilers())
    withClue(cspom) {
      cspom.expression("X") shouldBe Some(CSPOMConstant(true))
      cspom.expression("a[1]") shouldBe Some(CSPOMConstant(true))
    }
  }

  it should "correctly parse array" in {
    assert(fastparse.parse("[500.0,700.0,400.0,300.0]", FlatZincFastParser.array_expr(_)).isSuccess)
    //    match {
//      case p: Parsed.Failure=>
//        println(p.trace().longAggregateMsg)
//      case Parsed.Success(value, index) =>
//        println(value, index)
//    }

  }

}

