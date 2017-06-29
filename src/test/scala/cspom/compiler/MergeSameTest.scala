package cspom.compiler

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import cspom.CSPOM
import cspom.xcsp.XCSP3Parser
import org.scalatest.TryValues
import org.scalatest.concurrent.TimeLimits

class MergeSameTest extends FlatSpec with Matchers with TimeLimits with TryValues {
//
//  "MergeSame" should "not take too long to compile" in {
//    val url = XCSPParser.getClass.getResource("tsp-20-1_ext.xml.bz2")
//    CSPOM.load(url).map {
//      case cspom => failAfter(Span(10, Seconds)) {
//        CSPOMCompiler.compile(cspom, StandardCompilers() ++ StandardCompilers.improve())
//      }
//    } should be a 'success
//  }
//
  "MergeSame" should "not alter the problem" in {
    val url = Option(XCSP3Parser.getClass.getResource("Queens-0008-m1.xml.xz")).get
    CSPOM.load(url).map {
      case cspom => CSPOMCompiler.compile(cspom, Seq(MergeSame, MergeEq, RemoveUselessEq))
    } should be a 'success

  }
}