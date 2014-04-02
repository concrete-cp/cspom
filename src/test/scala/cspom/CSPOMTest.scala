package cspom

import org.junit.Assert._
import org.junit.Test
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import CSPOM._
import cspom.variable.BoolVariable

class CSPOMTest {

  @Test
  def variables(): Unit = {

    var vars: List[CSPOMVariable[Int]] = null

    val cspom = CSPOM { implicit problem =>
      vars = List(
        IntVariable(0 to 10) as "test1",
        IntVariable(10 to 20) as "test2",
        IntVariable(20 to 30) as "test3",
        IntVariable(30 to 40) as "test4")

      //vars foreach { cspom.addVariable(_) }

      ctr(CSPOMConstraint('dummy, vars))
    }
    assertTrue(vars sameElements cspom.namedExpressions.values)
    assertEquals(Some(vars(0)), cspom.expression("test1"))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def duplicateVariable(): Unit = {
    CSPOM { implicit problem =>
      IntVariable(0 to 10) as "Test"
      IntVariable(0 to 10) as "Test"
    }
  }

  @Test
  def boolVariables(): Unit = {
    val cspom = CSPOM { implicit problem =>
      ctr(CSPOMConstraint('dummy, Seq(new BoolVariable())))
      ctr(CSPOMConstraint('dummy, Seq(new BoolVariable())))
    }
    // Third is CSPOMConstant(true) !
    assertEquals(3, cspom.referencedExpressions.size)
  }

  @Test
  def constraints(): Unit = {

    var leq: CSPOMConstraint[Boolean] = null
    var v: List[CSPOMVariable[Int]] = null

    val cspom = CSPOM { implicit problem =>
      v = List(
        IntVariable(0 to 10),
        IntVariable(0 to 10));

      //v foreach { cspom.addVariable(_) }

      leq = ctr(CSPOMConstraint('leq, Seq(v(0), v(1))))
    }

    assertTrue(cspom.constraints(v(0)) contains leq)
    assertTrue(cspom.constraints(v(1)) contains leq)

    cspom.removeConstraint(leq)

    assertFalse(cspom.constraints.hasNext)
    assertFalse(cspom.constraints(v(0)) contains leq)
    assertFalse(cspom.constraints(v(1)) contains leq)

    assertTrue(cspom.referencedExpressions.isEmpty)
  }

  //  @Test(expected = classOf[IllegalArgumentException])
  //  def protectedVariable(): Unit = {
  //    val cspom = new CSPOM
  //    val v = List(
  //      CSPOMVariable.ofInterval("test1", 0, 10),
  //      CSPOMVariable.ofInterval("test2", 0, 10));
  //
  //    //v.foreach(cspom.addVariable)
  //
  //    val leq = new CSPOMConstraint('leq, v(0), v(1))
  //    cspom.ctr(leq);
  //    cspom.removeVariable(v(1))
  //  }
}