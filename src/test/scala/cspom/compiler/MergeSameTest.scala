package cspom.compiler

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.time.Seconds
import org.scalatest.time.Span
import cspom.xcsp.XCSPParser
import cspom.CSPOM
import org.scalatest.TryValues
import org.scalatest.concurrent.TimeLimits

class MergeSameTest extends FlatSpec with Matchers with TimeLimits with TryValues {

  "MergeSame" should "not take too long to compile" in {
    val url = XCSPParser.getClass.getResource("tsp-20-1_ext.xml.bz2")
    CSPOM.load(url).map {
      case cspom => failAfter(Span(10, Seconds)) {
        CSPOMCompiler.compile(cspom, StandardCompilers() ++ StandardCompilers.improve())
      }
    } should be a 'success
  }

  it should "not alter the problem" in {
    val url = Option(XCSPParser.getClass.getResource("queens-8.xml.xz")).get
    CSPOM.load(url).map {
      case cspom => CSPOMCompiler.compile(cspom, Seq(MergeSame, MergeEq, RemoveUselessEq))
    } should be a 'success

  }
}