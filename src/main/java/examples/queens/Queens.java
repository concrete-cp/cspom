package examples.queens;

import java.text.ParseException;

import cspom.CSPOM;
import cspom.DuplicateVariableException;
import cspom.variable.CSPOMVariable;

public class Queens {

	private static final int SIZE = 8;

	private Queens() {
	}

	public static CSPOM queens(int nbQueens) throws ParseException {

		CSPOM problem = new CSPOM();

		CSPOMVariable[] queens = new CSPOMVariable[nbQueens];

		for (int i = nbQueens; --i >= 0;) {
			try {
				queens[i] = problem.var("q" + i, 0, nbQueens);
			} catch (DuplicateVariableException e) {
				throw new IllegalStateException(e);
			}
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
		System.out.println(queens(SIZE).toGML());
	}
}