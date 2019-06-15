package cspom.compiler

import cspom.CSPOM.ctr
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint, UNSATException}
import org.scalatest.{FlatSpec, Matchers, TryValues}

import scala.reflect.runtime.universe._

class ACCSETest extends FlatSpec with Matchers with TryValues {

  def sum(variables: SimpleExpression[Int]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    sumProd(variables.map((1, _)): _*)
  }

  def sumProd(coefVar: (Int, SimpleExpression[Int])*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    val (coefs, vars) = coefVar.unzip
    problem.defineInt(result => linear(result +: vars, -1 +: coefs, "eq", 0))
  }

  def linear[A: TypeTag](vars: Seq[SimpleExpression[A]], coefs: Seq[Int], mode: String, constant: Int): CSPOMConstraint[Boolean] =
    CSPOMConstraint("sum")(CSPOM.constantSeq(coefs), CSPOM.seq2CSPOMSeq(vars), CSPOMConstant(constant)) withParam ("mode" -> mode)

  def minus(e: SimpleExpression[Int], other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
    sumProd((1, e), (-1, other))
  }


//  "ACCSE" should "greatly improve bad GR model" in {
//    val ticks = 3
//    val max = ticks * ticks
//
//
//    val problem = CSPOM { implicit problem =>
//      val variables = for (i <- 1 to ticks) yield IntVariable(1 to max) as s"T$i"
//
//      for (Seq(xi, xj) <- variables.sliding(2)) {
//        ctr(CSPOMConstraint('lt)(xi, xj))
//      }
//
//      for (
//        xi <- variables; xj <- variables if xi != xj;
//        xk <- variables; xl <- variables if xk != xl &&
//          (xi != xk || xj != xl)
//      ) {
//        val d1 = minus(xi, xj)
//        val d2 = minus(xk, xl)
//        ctr(CSPOMConstraint('ne)(d1, d2))
//      }
//
//      goal(Minimize(variables.last))
//    }
//
//    CSPOMCompiler.compile(problem, Seq(MergeEq, SumEq, SumSE, MergeSameSum)).get
//    // println(problem)
//    val nbDiffs = ticks * (ticks - 1)
//    val nbConstraints =
//      (ticks - 1) + //lt constraints
//        nbDiffs + // diff definitions
//        nbDiffs * (nbDiffs - 1) // alldiff (constraints are posted on both sides)
//
//    problem.constraints should have size nbConstraints
//
//  }

  "ACCSE" should "allow CSPOM to detect contradiction" in {
    val problem = CSPOM { implicit problem =>
      val x = IntVariable.free() as "X"
      val y = IntVariable.free() as "Y"
      val z = IntVariable.free() as "Z"

      ctr(linear(Seq(x, y, z), Seq(1, 1, 1), "le", 10))
      ctr(linear(Seq(x, y, z), Seq(-1, -1, -1), "lt", -11))
    }


    val c = CSPOMCompiler.compile(problem, Seq(SumDomains, SumSE))
    // println(c)
    c.failure.exception shouldBe an[UNSATException]
  }

  it should "allow CSPOM to detect sum subexpressions" in {
    val problem = CSPOM { implicit problem =>
      val x = IntVariable(-10 to 10) as "X"
      val y = IntVariable(-10 to 10) as "Y"
      val z = IntVariable(-10 to 10) as "Z"
      val w = IntVariable(-10 to 10) as "W"

      ctr(CSPOMConstraint("eq")(sumProd((2, x), (3, y), (1, z)), CSPOMConstant(5)))
      ctr(CSPOMConstraint("eq")(sumProd((4, x), (6, y), (1, w)), CSPOMConstant(5)))
    }


    CSPOMCompiler.compile(problem, Seq(ReifiedConj, SimplClause, UnaryClause, MergeEq, SumSE)).get

    problem.constraints.toSeq should have size 3
  }

  //  it should "allow CSPOM to detect bool subexpressions" in {
  //    val problem = CSPOM { implicit problem =>
  //      val x = new BoolVariable() as "X"
  //      val y = new BoolVariable() as "Y"
  //      val z = new BoolVariable() as "Z"
  //      val w = new BoolVariable() as "W"
  //
  //      ctr(clause(x)(y, z))
  //      ctr(clause(x)(w, y))
  //    }
  //
  //    Solver(problem).get
  //
  //    problem.constraints.toSeq should have size 5
  //  }

  //  it should "allow CSPOM to detect min subexpressions" in {
  //    val problem = CSPOM { implicit problem =>
  //      val x = IntVariable(-10 to 10) as "X"
  //      val y = IntVariable(-10 to 10) as "Y"
  //      val z = IntVariable(-10 to 10) as "Z"
  //      val w = IntVariable(-10 to 10) as "W"
  //
  //      ctr(CSPOMSeq(x, y, z).cmin === 1)
  //      ctr(CSPOMSeq(x, y, w).cmin === 2)
  //
  //      ctr(CSPOMSeq(x, y, z).cmax === 3)
  //      ctr(CSPOMSeq(x, y, w).cmax === 4)
  //    }
  //
  //    Solver(problem).get
  //
  //    problem.constraints.toSeq should have size 6
  //  }

  it should "scale" in {
    val problem = CSPOM.load(classOf[ACCSETest].getResource("../flatzinc/vrp-P-n51-k10.vrp.fzn.xz"))
      .flatMap(CSPOMCompiler.compile(_, FZPatterns()))
      .get

    val compiled = CSPOMCompiler.compile(problem, Seq(SumSE)).get

    // println(compiled)
  }

  it should "scale on long constraints" in {
    val problem = CSPOM { implicit problem =>
      val vars = Seq.tabulate(10)(i => IntVariable(0 to 10) as s"x$i")
      for( _ <- 0 until 10) {
        ctr(linear(vars, Seq.fill(10)(10), "lt", 10))
      }
    }

    val compiled = CSPOMCompiler.compile(problem, Seq(SumSE)).get
    println(compiled)
  }

  it should "scale more" in {
    val cspom = CSPOM.load(CSPOM.getClass.getResource("flatzinc/oocsp_racks-oocsp_racks_100_r1_cc.fzn.xz")).get
    CSPOMCompiler.compile(cspom, FZPatterns()).get
    CSPOMCompiler.compile(cspom, Seq(MergeEq, SumDuplicates, ReifiedConj, SimplClause, UnaryClause, SumSE)).get

  }

  it should "find correct intersection" in {
    val vars = IndexedSeq.tabulate(4)(i => IntVariable(i to i)).sortBy(_.hashCode())

    val subExp1 = List((vars(0), 12), (vars(1), 13), (vars(2), 9), (vars(3), 15))
    val subExp2 = List((vars(0), 8), (vars(1), 9), (vars(2), 6), (vars(3), 10))

    val pair = SumSE.canonize(List(subExp1(0), subExp1(3)))

    SumSE.intersect(subExp1, subExp2, pair) should contain theSameElementsAs List((vars(0), 4), (vars(2), 3), (vars(3), 5))
  }


}
