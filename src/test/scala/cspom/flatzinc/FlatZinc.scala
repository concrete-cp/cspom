package cspom.flatzinc

import scala.util.parsing.combinator.JavaTokenParsers
import org.scalatest.FlatSpec
import org.scalatest.TryValues
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.VariableNames
import cspom.compiler.Delta

class FlatZinc extends FlatSpec with TryValues {

  val url = classOf[FlatZinc].getResource("photo.fzn")

  "FlatZinc parser" should s"parse $url" in {
    val (cspom, variables) = FlatZincParser(url.openStream).success.get

    // CSPOMCompiler.compile(cspom, FZPatterns()).get

    val Seq(fz) = FZPatterns()
    val vn = new VariableNames(cspom)
    val constraint = cspom.constraints.next
    val delta = for (data <- fz.mtch(constraint, cspom)) yield {
      val delta = fz.compile(constraint, cspom, data)
      println(delta.toString(vn))
    }

    println(cspom)

  }

}

