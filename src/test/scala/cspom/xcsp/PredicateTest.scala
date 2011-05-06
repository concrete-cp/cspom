package cspom.xcsp;

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
    println(predicate.types)
  }

}
