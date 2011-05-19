package scalaexamples.queens;

import cspom.CSPOM
import cspom.variable.CSPOMVariable;
import cspom.compiler.ProblemCompiler

final object Queens {

  val SIZE = 8;

  def queens(nbQueens: Int) = {

    val problem = new CSPOM();

    val queens = (1 to nbQueens) map (i => problem.interVar("Q" + i, 1, nbQueens)) toIndexedSeq

    for (Seq(i, j) <- (0 to nbQueens - 1).combinations(2)) {
      problem.ctr("neq(" + queens(i) + " , " + queens(j) + ")");
      problem.ctr("neq(abs(sub(" + queens(i) + "," + queens(j) + ")), " +
        math.abs(i - j) + ")");

    }

    problem;
  }

  def main(args: Array[String]) {
    println(ProblemCompiler.compile(queens(SIZE)));
  }
}