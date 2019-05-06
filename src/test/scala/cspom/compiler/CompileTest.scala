package cspom.compiler


import cspom.{CSPOM, CSPOMConstraint}
import cspom.variable.IntVariable
import org.scalatest.FlatSpec
import org.scalatest.TryValues
import org.scalatest.Matchers
import CSPOM._

final class CompileTest extends FlatSpec with Matchers with TryValues {
  //  "CSPOMCompiler" should "compile zebra" in {
  //    compileTest("zebra.xml.xz");
  //  }
  //
  //  def compileTest(fn: String) {
  //    CSPOM.load(classOf[CompileTest].getResource(fn)).map {
  //      case cspom => CSPOMCompiler.compile(cspom, StandardCompilers())
  //    } should be a 'success
  //
  //    CSPOM.load(classOf[CompileTest].getResource(fn)).map {
  //      case cspom => CSPOMCompiler.compile(cspom, StandardCompilers() ++ StandardCompilers.improve())
  //    } should be a 'success
  //  }


  "MergeEq" should "merge trues" in {
    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable(1, 2, 3) as "v0"
      val v1 = IntVariable(2, 3, 4) as "v1"
      //      val v2 = IntVariable(1, 2, 3) as "v2"
      //      val v3 = IntVariable(1, 2, 3) as "v3"

      val b = problem.defineBool(r => CSPOMConstraint(r)('not)(v0 === v1))
      ctr(b)

    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq, NegToCNF, SimplClause, UnaryClause)) //, Reversed, AllDiff))

    //println(cspom)
    withClue(cspom) {
      cspom.constraints should have size 1

      assert(cspom.constraints.exists {
        c => c.function == 'eq && c.result.isFalse
      })
    }
  }


}
