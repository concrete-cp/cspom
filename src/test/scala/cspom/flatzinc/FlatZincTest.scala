package cspom.flatzinc

import org.scalatest.FlatSpec
import org.scalatest.TryValues
import cspom.VariableNames

class FlatZincTest extends FlatSpec with TryValues {

  val url = classOf[FlatZincTest].getResource("photo.fzn")

  "FlatZinc parser" should s"parse $url" in {
    val (cspom, variables) = FlatZincParser(url.openStream).success.get

    // CSPOMCompiler.compile(cspom, FZPatterns()).get

    val Seq(fz) = FZPatterns()
    val vn = new VariableNames(cspom)
    val constraint = cspom.constraints.next
    val delta = for (data <- fz.mtch(constraint, cspom)) yield {
      val delta = fz.compile(constraint, cspom, data)
      //println(delta.toString(vn))
    }

    //println(cspom)

  }

}

