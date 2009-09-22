package examples.queens;

import java.text.ParseException;

import cspom.Problem;
import cspom.variable.Variable;

public class Queens {

	public static Problem queens(int nbQueens) throws ParseException {

		Variable[] queens = new Variable[nbQueens];

		for (int i = nbQueens; --i >= 0;) {
			queens[i] = new Variable(0, nbQueens);
		}

		Problem problem = new Problem(nbQueens + "-queens");

		for (int i = nbQueens; --i >= 0;) {
			for (int j = i; --j >= 0;) {
				problem.addConstraint("and(neq(x0,x1),neq(abs(x0-x1),"
						+ (j - i) + "))", queens[i], queens[j]);
			}
		}

		return problem;
	}

	public static void main(String[] args) throws ParseException {
		queens(8);
	}
}