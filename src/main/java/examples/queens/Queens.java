package examples.queens;

import java.text.ParseException;

import cspom.Problem;
import cspom.variable.Variable;

public class Queens {

	public static Problem queens(int nbQueens) throws ParseException {

		Problem problem = new Problem(nbQueens + "-queens");

		Variable[] queens = new Variable[nbQueens];

		for (int i = nbQueens; --i >= 0;) {
			queens[i] = problem.var("q" + i, 0, nbQueens);
		}

		for (int i = nbQueens; --i >= 0;) {
			for (int j = i; --j >= 0;) {
				problem.ctr("neq(" + queens[i] + " , " + queens[j] + ")");
				problem.ctr("neq(abs(sub(" + queens[i] + "," + queens[j]
						+ ")), " + Math.abs(i - j) + ")");
			}
		}

		return problem;
	}

	public static void main(String[] args) throws ParseException {
		System.out.println(queens(8).toGML());
	}
}