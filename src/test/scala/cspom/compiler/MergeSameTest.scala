package cspom.compiler

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.time.Seconds
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.Span
import cspom.xcsp.XCSPParser
import cspom.CSPOM

class MergeSameTest extends FlatSpec with Matchers with Timeouts {

  "MergeSame" should "not take too long to compile" in {
    val url = XCSPParser.getClass.getResource("tsp-20-1_ext.xml.bz2")
    val (cspom, _) = CSPOM.load(url)

    failAfter(Span(10, Seconds)) {
      ProblemCompiler.compile(cspom, StandardCompilers() ++ StandardCompilers.improve())
    }
  }

  it should "not alter the problem" in {
    val url = XCSPParser.getClass.getResource("queens-8.xml")
    val (cspom, _) = CSPOM.load(url)

    ProblemCompiler.compile(cspom, Seq(MergeSame, MergeEq, RemoveUselessEq))

  }
}