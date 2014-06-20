package cspom.compiler;

import cspom.xcsp.{ ParserTest, XCSPParser }
import cspom.CSPOM
import org.scalatest.FlatSpec

final class CompileTest extends FlatSpec {
  "ProblemCompiler" should "compile zebra" in {
    compileTest("zebra.xml");
  }

  it should "compile queens-12" in {
    compileTest("queens-12.xml");
  }

  it should "compile scen11-f12" in {
    compileTest("scen11-f12.xml.bz2");
  }

  it should "compile crosswordm2" in {
    compileTest("crossword-m2-debug-05-01.xml");
  }

  it should "compile lexHerald" in {
    compileTest("normalized-crossword-m1-lex-15-04.xml.bz2");
  }

  it should "compile crosswordm1" in {
    compileTest("crossword-m1-debug-05-01.xml")
  }

  def compileTest(fn: String) {
    {
      val cspom = CSPOM.load(classOf[CompileTest].getResource(fn))._1;
      ProblemCompiler.compile(cspom, StandardCompilers())
    }
    {
      val cspom = CSPOM.load(classOf[CompileTest].getResource(fn))._1;
      ProblemCompiler.compile(cspom, StandardCompilers() ++ StandardCompilers.improve())
    }
  }

}
