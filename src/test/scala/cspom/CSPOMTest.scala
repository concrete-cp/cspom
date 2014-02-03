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
    val (cspom: CSPOM, vars) = CSPOM withResult {
      val vars = List(
        IntVariable.ofInterval(0, 10) as "test1",
        IntVariable.ofInterval(10, 20) as "test2",
        IntVariable.ofInterval(20, 30) as "test3",
        IntVariable.ofInterval(30, 40) as "test4")

      //vars foreach { cspom.addVariable(_) }

      ctr(new CSPOMConstraint('dummy, vars))

      vars
    }
    assertTrue(vars sameElements cspom.namedExpressions.values)
    assertEquals(Some(vars(0)), cspom.expression("test1"))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def duplicateVariable(): Unit = {
    CSPOM {
      IntVariable.ofInterval(0, 10) as "Test"
      IntVariable.ofInterval(0, 10) as "Test"
    }
  }

  @Test
  def boolVariables(): Unit = {
    val cspom = CSPOM {
      ctr(new CSPOMConstraint('dummy, new BoolVariable()))
      ctr(new CSPOMConstraint('dummy, new BoolVariable()))
    }
    // Third is CSPOMTrue !
    assertEquals(3, cspom.referencedExpressions.size)
  }

  @Test
  def constraints(): Unit = {
    val (cspom, (v, leq)) = CSPOM withResult {
      val v = List(
        IntVariable.ofInterval(0, 10),
        IntVariable.ofInterval(0, 10));

      //v foreach { cspom.addVariable(_) }

      val leq = ctr(new CSPOMConstraint('leq, v(0), v(1)))

      (v, leq)

    }

    assertTrue(cspom.constraints(v(0)) contains leq)
    assertTrue(cspom.constraints(v(1)) contains leq)

    cspom.removeConstraint(leq)

    assertEquals(Set(), cspom.constraints)
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