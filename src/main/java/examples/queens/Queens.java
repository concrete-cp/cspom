package examples.queens;

import cspom.CSPOM;
import cspom.compiler.PredicateParseException;
import cspom.variable.CSPOMVariable;

public final class Queens {

    private static final int SIZE = 8;

    private Queens() {
    }

    public static CSPOM queens(final int nbQueens) throws PredicateParseException {

        final CSPOM problem = new CSPOM();

        final CSPOMVariable[] queens = new CSPOMVariable[nbQueens];

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

    public static void main(final String[] args) throws PredicateParseException {
        System.out.println(queens(SIZE).toGML());
    }
}