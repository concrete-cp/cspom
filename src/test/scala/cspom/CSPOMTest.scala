package cspom

import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import CSPOM._
import cspom.variable.BoolVariable
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.OptionValues
import cspom.variable.CSPOMSeq
import cspom.compiler.MergeEq
import cspom.compiler.CSPOMCompiler
import cspom.variable.BoolVariable
import cspom.variable.BoolExpression
import cspom.variable.IntExpression
import cspom.variable.CSPOMExpression
import cspom.compiler.MergeSame
import cspom.variable.CSPOMConstant

class CSPOMTest extends FlatSpec with Matchers with OptionValues {

  "CSPOM" should "accept variables" in {
    var vars: List[CSPOMVariable[Int]] = null

    val cspom = CSPOM { implicit problem =>
      vars = List(
        IntVariable(0 to 10) as "test1",
        IntVariable(10 to 20) as "test2",
        IntVariable(20 to 30) as "test3",
        IntVariable(30 to 40) as "test4")

    }

    vars should contain theSameElementsAs cspom.expressionsWithNames.map(_._2)

    cspom.expression("test1").value shouldBe vars(0)
  }

  it should "throw exception in case of same names" in {
    an[IllegalArgumentException] should be thrownBy
      CSPOM { implicit problem =>
        IntVariable(0 to 10) as "Test"
        IntVariable(0 to 10) as "Test"
      }
  }

  it should "accept and reference bool variables" in {
    val cspom = CSPOM { implicit problem =>
      ctr(CSPOMConstraint('dummy)(new BoolVariable()))
      ctr(CSPOMConstraint('dummy)(new BoolVariable()))
    }
    // Third is CSPOMConstant(true) !
    cspom.referencedExpressions should have size 3
  }

  it should "reference and dereference constraints" in {
    var leq: CSPOMConstraint[Boolean] = null
    var v: List[CSPOMVariable[Int]] = null

    val cspom = CSPOM { implicit problem =>
      v = List(
        IntVariable(0 to 10),
        IntVariable(0 to 10));

      //v foreach { cspom.addVariable(_) }

      leq = ctr(CSPOMConstraint('leq)(v(0), v(1)))
    }

    cspom.constraints(v(0)) should contain(leq)
    cspom.constraints(v(1)) should contain(leq)

    cspom.removeConstraint(leq)

    cspom.constraints should not be ('hasNext)

    cspom.constraints(v(0)) should not contain (leq)
    cspom.constraints(v(1)) should not contain (leq)

    cspom.referencedExpressions shouldBe empty

  }

  it should "correctly replace variables" in {
    val cspom = CSPOM { implicit problem =>
      val x = IntVariable(0 to 10) as "X"
      val y = IntVariable(10 to 20) as "Y"
      Seq(x, y) as "T"
    }

    val x = cspom.expression("X").get

    val y = cspom.expression("Y").get
    val rx = IntVariable(20 to 30)

    cspom.replaceExpression(x, rx)
    val t = cspom.expression("T").get

    t shouldBe CSPOMSeq(rx, y)
    cspom.expression("X").value shouldBe rx
    cspom.expression("T[0]").value shouldBe rx
    cspom.expression("T[1]").value shouldBe y
    cspom.referencedExpressions should contain theSameElementsAs Seq(rx, y, t)
    cspom.namesOf(x) shouldBe 'empty
    cspom.namesOf(rx) should contain theSameElementsAs Seq("X", "T[0]")
    cspom.namesOf(y) should contain theSameElementsAs Seq("Y", "T[1]")

    cspom.replaceExpression(t, x)
    cspom.expression("X").value shouldBe rx
    cspom.expression("T").value shouldBe x
    cspom.expression("T[0]") shouldBe None
    cspom.referencedExpressions should contain theSameElementsAs Seq(rx, y, x)
    cspom.namesOf(x) should contain theSameElementsAs Seq("T")
    cspom.namesOf(rx) should contain theSameElementsAs Seq("X")
    cspom.namesOf(y) should contain theSameElementsAs Seq("Y")

  }

  it should "correctly merge variables" in {
    val cspom = CSPOM { implicit problem =>
      val x = IntVariable(0 to 10) as "X"
      val y = IntVariable(10 to 20) as "Y"

      val array = IndexedSeq.fill(2)(IntVariable.free()) as "Array"

      ctr(x === array(0))
      ctr(y === array(1))
    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq))

    //    println(cspom)
    //
    //    println("namedExpressions")
    //    println(cspom.namedExpressions.mkString("\n"))
    //    println("\nexpressionNames")
    //    println(cspom.expressionNames.mkString("\n"))
    //    println("\ncontainers")
    //    println(cspom.containers.mkString("\n"))
  }

  it should "correctly post constraint graphs" in {
    val cspom = CSPOM { implicit problem =>
      //      val month = IntVariable(5 to 8) as "month"
      //      val day = IntVariable(14 to 19) as "day"
      //
      //      ctr(Seq(month, day) in Seq(Seq(5, 15), Seq(6, 17), Seq(7, 14), Seq(8, 14), Seq(5, 16), Seq(6, 18), Seq(7, 16), Seq(8, 15), Seq(5, 19), Seq(8, 17)))
      //
      //      val albert1 = for (i <- 5 to 8) yield for (j <- 14 to 19) yield new BoolVariable() as s"albert1 $i/$j"
      def lt(x: CSPOMExpression[_], y: CSPOMExpression[_]) = (x, y) match {
        case (BoolExpression(xi), BoolExpression(yi)) => problem.isBool('clause, Seq(Seq(yi), Seq(xi)))
        case (IntExpression(xi), IntExpression(yi))   => problem.isBool('lt, Seq(xi, yi))
      }

      val x = for (i <- 0 until 5) yield new BoolVariable() as s"X$i"
      val y = for (i <- 0 until 5) yield new BoolVariable() as s"Y$i"

      val n = x.size
      require(y.size == n)

      val disjunction = Seq(
        lt(x(0), y(0)),
        problem.isBool('and, (0 until n).map(i => x(i) === y(i)))) ++
        (0 until n - 1).map { i =>
          val conjunction =
            (0 until i).map(j => x(j) === y(j)) :+ lt(x(i + 1), y(i + 1))
          problem.isBool('and, conjunction)
        }

      ctr(problem.isBool('or, disjunction))

    }

    cspom.getPostponed shouldBe empty

    CSPOMCompiler.compile(cspom, Seq(MergeEq, MergeSame))
  }

  it should "correctly post constraint graphs with named expressions" in {
    val cspom = CSPOM { implicit problem =>
      val v0 = IntVariable(1 to 3)
      val v1 = IntVariable(2 to 4)

      val r = problem.defineInt(r => CSPOMConstraint('sum)(Seq(v0, v1, r), 0))

      val r2 = problem.isInt('abs, Seq(r)) as "r2"

    }

    withClue(cspom) {
      cspom.getPostponed shouldBe empty
      cspom.constraints should have size 2
      cspom.referencedExpressions should have size 6
    }
  }
}