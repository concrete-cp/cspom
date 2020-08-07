package cspom.compiler


import cspom.CSPOM._
import cspom.util.IntInterval
import cspom.variable.{CSPOMSeq, IntVariable, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class CompileTest extends AnyFlatSpec with Matchers with TryValues {
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

      val b = problem.defineBool(r => CSPOMConstraint(r)("not")(v0 === v1))
      ctr(b)

    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq, NegToCNF, SimplClause, UnaryClause)) //, Reversed, AllDiff))

    //println(cspom)
    withClue(cspom) {
      cspom.constraints should have size 1

      assert(cspom.constraints.exists {
        c => c.function == "eq" && c.result.isFalse
      })
    }
  }

  it should "replace properly" in {
    var a1, a2: SimpleExpression[Int] = null
    var eq, le1, le2: CSPOMConstraint[_] = null

    val problem = CSPOM { implicit problem =>
      val x = IntVariable(0 to 1) as "X"


      a1 = problem.defineInt(a =>
        CSPOMConstraint("sum")(CSPOMSeq(a, x), CSPOMSeq(-1, 1), 0) withParam "mode" -> "eq")

      a2 = problem.defineInt { a =>
        eq = CSPOMConstraint("eq")(a1, a)
        eq
      }

      le1 = CSPOMConstraint("sum")(CSPOMSeq(a1, 10), CSPOMSeq(1, -1), 0) withParam "mode" -> "le"
      le2 = CSPOMConstraint("sum")(CSPOMSeq(11, a2), CSPOMSeq(1, -1), 0) withParam "mode" -> "le"

      ctr(le1)
      ctr(le2)
    }
    // println(problem)


    val delta2 = ConstraintCompiler.replaceCtr(le1, CSPOMConstraint("sum")(CSPOMSeq(a1), CSPOMSeq(1), 10) withParam "mode" -> "le", problem)
//    println(delta2.toString(problem))
//    println(problem)
//    println("==========")

    val delta3 = ConstraintCompiler.replaceCtr(le2, CSPOMConstraint("sum")(CSPOMSeq(a2), CSPOMSeq(-1), -11) withParam "mode" -> "le", problem)
//    println(delta3.toString(problem))
//    println(problem)
//    println("==========")

    val delta4 = MergeEq.compile(eq, problem)
//    println(delta4.toString(problem))
//    println(problem)
//    println("==========")

    val delta5 = ConstraintCompiler.replace(a1, IntVariable(IntInterval.atMost(10)), problem)
//    println(delta5.toString(problem))
//    println(problem)
    //withClue(delta5.toString(problem)) {
      problem.referencedExpressions should not contain a1
    //}
  }


}
