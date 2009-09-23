package examples.queens;

import java.text.ParseException;

import cspom.Problem;
import cspom.variable.Variable;

public class Queens {

	public static Problem queens(int nbQueens) throws ParseException {
		
		Problem problem = new Problem(nbQueens + "-queens");
		
		Variable[] queens = new Variable[nbQueens];

		for (int i = nbQueens; --i >= 0;) {
			queens[i] = problem.var(0, nbQueens);
		}

		for (int i = nbQueens; --i >= 0;) {
			for (int j = i; --j >= 0;) {
				problem.ctr(queens[i] + " != " + queens[j]);
				problem.ctr("abs(" + queens[i] + "-" + queens[j] + ") != "
						+ (j - i));
			}
		}

		return problem;
	}

	public static void main(String[] args) throws ParseException {
		queens(8);
	}
}