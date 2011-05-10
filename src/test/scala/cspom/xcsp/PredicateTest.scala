package cspom.xcsp;

import cspom.variable.CSPOMVariable
import cspom.CSPOM
import org.junit.Assert._
import org.junit.Test

final class PredicateTest {

  @Test
  def test() {
    val predicate = new Predicate("int X0 int X1 int X2 int X3",
      "or(ne(X0,X1),or(ne(X2,X3)))")

    assertEquals(Seq("X0", "X1", "X2", "X3"), predicate.parameters)
    assertEquals(Map(
      "X0" -> "int",
      "X1" -> "int",
      "X2" -> "int",
      "X3" -> "int"), predicate.types)

    val variables = Seq(
      CSPOMVariable.of("V0", 0),
      CSPOMVariable.of("V1", 1),
      CSPOMVariable.of("V2", 2))

    assertEquals("or(ne(V0,V1),or(ne(V2,5)))", predicate.applyParameters("V0 V1 V2 5", variables))
  }

}
