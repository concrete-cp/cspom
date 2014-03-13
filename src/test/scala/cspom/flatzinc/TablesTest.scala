package cspom.flatzinc

import org.junit.Test
import org.junit.Assert._
import cspom.variable.IntVariable
import cspom.variable.CSPOMSeq
import cspom.extension.MDD

class TablesTest {

  @Test
  def test(): Unit = {
    val as = Seq(
      IntVariable(1 to 3),
      IntVariable(3 to 4),
      IntVariable(5 to 7))

    val b = IntVariable(1 to 3)

    val c = IntVariable(1 to 7)

    val mdd = Tables.elementVar(new CSPOMSeq(as, 2 to 4))

    mdd.foreach(println)
    assertEquals(54, mdd.size)

  }
}