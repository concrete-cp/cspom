package cspom.compiler

import cspom.CSPOM
import org.scalatest.{FlatSpec, Matchers}

class ACCSETest extends FlatSpec with Matchers {
  "ACCSE" should "scale" in {
    val problem = CSPOM.load(classOf[ACCSETest].getResource("../flatzinc/vrp-P-n51-k10.vrp.fzn.xz"))
      .flatMap(CSPOMCompiler.compile(_, FZPatterns()))
      .get

    val compiled = CSPOMCompiler.compile(problem, Seq(SumSE)).get

   // println(compiled)
  }

}
