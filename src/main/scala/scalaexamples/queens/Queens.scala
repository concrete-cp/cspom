package scalaexamples.queens;

import cspom.CSPOM
import CSPOM._
import cspom.variable.CSPOMVariable;
import cspom.compiler.ProblemCompiler

final object Queens {

  val SIZE = 8;

  def queens(nbQueens: Int) = CSPOM {

    val queens = (1 to nbQueens) map (i => interVar("Q" + i, 1, nbQueens)) toIndexedSeq

    for (Seq(i, j) <- (0 until nbQueens).combinations(2)) {
      ctr(queens(i) ≠ queens(j))
      ctr('abs(queens(i) - queens(j)) ≠ math.abs(i - j))
    }

  }

  def main(args: Array[String]) {
    val p = queens(SIZE)
    //ProblemCompiler.compile(p)
    println(p);
  }
}