package cspom.compiler;

import cspom.CSPOM
import org.scalatest.FlatSpec
import org.scalatest.TryValues
import org.scalatest.Matchers

final class CompileTest extends FlatSpec with Matchers with TryValues {
//  "CSPOMCompiler" should "compile zebra" in {
//    compileTest("zebra.xml.xz");
//  }

  def compileTest(fn: String) {
    CSPOM.load(classOf[CompileTest].getResource(fn)).map {
      case cspom => CSPOMCompiler.compile(cspom, StandardCompilers())
    } should be a 'success

    CSPOM.load(classOf[CompileTest].getResource(fn)).map {
      case cspom => CSPOMCompiler.compile(cspom, StandardCompilers() ++ StandardCompilers.improve())
    } should be a 'success
  }

}
