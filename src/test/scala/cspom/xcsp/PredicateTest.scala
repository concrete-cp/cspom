package cspom.xcsp;

import org.junit.Assert._
import org.junit.Test
import cspom.constraint.Predicate
import cspom.variable.CSPOMVariable

final class PredicateTest {

  @Test
  def test() {
    val predicate = new XCSPPredicate("int X0 int X1 int X2 int X3",
      "or(ne(X0,X1),or(ne(X2,X3)))")

    assertEquals(Seq("X0", "X1", "X2", "X3"), predicate.parameters)
    assertEquals(Map(
      "X0" -> "int",
      "X1" -> "int",
      "X2" -> "int",
      "X3" -> "int"), predicate.types)

    val variables = Seq(
      CSPOMVariable.ofInt("V0", 0, 1),
      CSPOMVariable.ofInt("V1", 1, 2),
      CSPOMVariable.ofInt("V2", 2, 3))

    assertEquals("or(ne(V0,V1),or(ne(V2,5)))", predicate.applyParameters("V0 V1 V2 5", variables))
  }

  @Test
  def test3() {
    val predicate = new XCSPPredicate("int X0 int X1 int X2",
      "or(ne(X0,X1),or(ne(X0,X2)))")

    assertEquals(Seq("X0", "X1", "X2"), predicate.parameters)
    assertEquals(Map(
      "X0" -> "int",
      "X1" -> "int",
      "X2" -> "int"), predicate.types)

    val variables = Seq(
      CSPOMVariable.ofInt("V0", 0, 1),
      CSPOMVariable.ofInt("V1", 1, 2),
      CSPOMVariable.ofInt("V2", 2, 3))

    assertEquals("or(ne(V0,V1),or(ne(V0,V2)))", predicate.applyParameters("V0 V1 V2", variables))
  }

  @Test
  def test4() {
    val predicate = new XCSPPredicate("int X0 int X1 int X2 int X3 int X4 int X5 int X6 int X7 int X8 int X9 int X10 int X11",
      "or(ne(X0,X1),or(ne(X2,X3),or(ne(X4,X5),or(ne(X6,X7),or(ne(X8,X9),ne(X10,X11))))))");

    val variables = Seq(
      CSPOMVariable.ofInt("V3", 0, 1),
      CSPOMVariable.ofInt("V17", 1, 2),
      CSPOMVariable.ofInt("V31", 2, 3),
      CSPOMVariable.ofInt("V46", 0, 1),
      CSPOMVariable.ofInt("V56", 0, 1),
      CSPOMVariable.ofInt("V66", 1, 2),
      CSPOMVariable.ofInt("V126", 2, 3),
      CSPOMVariable.ofInt("V136", 0, 1),
      CSPOMVariable.ofInt("V146", 1, 2),
      CSPOMVariable.ofInt("V161", 2, 3),
      CSPOMVariable.ofInt("V175", 0, 1),
      CSPOMVariable.ofInt("V189", 1, 2))

    assertEquals(
      "or(ne(V3,V126),or(ne(V17,V136),or(ne(V31,V146),or(ne(V46,V161),or(ne(V56,V175),ne(V66,V189))))))",
      predicate.applyParameters("V3 V126 V17 V136 V31 V146 V46 V161 V56 V175 V66 V189", variables))

  }

}
