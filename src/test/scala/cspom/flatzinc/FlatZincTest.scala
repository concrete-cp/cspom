package cspom.flatzinc

import org.scalatest.FlatSpec
import org.scalatest.TryValues

import org.scalatest.Matchers
import cspom.variable.CSPOMConstant
import cspom.compiler.StandardCompilers
import cspom.compiler.CSPOMCompiler
import cspom.CSPOM

class FlatZincTest extends FlatSpec with Matchers {

  val url = classOf[FlatZincTest].getResource("1d_rubiks_cube.fzn.xz")

  "FlatZinc parser" should s"parse $url" in {
    val cspom = CSPOM.load(url, FlatZincFastParser).get

    // CSPOMCompiler.compile(cspom, FZPatterns()).get

    //val Seq(fz) = FZPatterns()

    val constraint = cspom.constraints.next
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

}

