package cspom;

import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.ParseException;
import java.util.Arrays;
import java.util.List;

import javax.script.ScriptException;

import org.junit.Test;

import cspom.constraint.Constraint;
import cspom.predicate.Predicate;
import cspom.predicate.PredicateConstraint;
import cspom.variable.Variable;

public class ProblemTest {

	@Test
	public void testSemiCompilePredicates() throws ScriptException,
			FileNotFoundException, ParseException, IOException {
		final Problem problem = Problem.load(this.getClass().getResource("cc-6-6-2.xml.bz2"));

		final PredicateConstraint constraint = (PredicateConstraint) problem
				.getConstraints().iterator().next();
		final Predicate predicate = constraint.getRelation();

		final Variable var0 = constraint.getScope().get(0);
		final Variable var1 = constraint.getScope().get(1);
		final Variable var2 = constraint.getScope().get(2);
		final Variable var3 = constraint.getScope().get(3);

		final boolean[][][][] initResults = new boolean[var0.getDomain()
				.getNbValues()][var1.getDomain().getNbValues()][var2
				.getDomain().getNbValues()][var3.getDomain().getNbValues()];
		final Number[] tuple = new Number[4];
		for (int i = var0.getDomain().getNbValues(); --i >= 0;) {
			tuple[0] = var0.getDomain().getValues().get(i);
			for (int j = var1.getDomain().getNbValues(); --j >= 0;) {
				tuple[1] = var1.getDomain().getValues().get(j);
				for (int k = var2.getDomain().getNbValues(); --k >= 0;) {
					tuple[2] = var2.getDomain().getValues().get(k);
					for (int l = var3.getDomain().getNbValues(); --l >= 0;) {
						tuple[3] = var3.getDomain().getValues().get(l);
						initResults[i][j][k][l] = constraint.evaluate(tuple);
					}
				}
			}
		}

		problem.semiCompilePredicates();

		assertNotSame(constraint.getRelation(), predicate);

		for (int i = var0.getDomain().getNbValues(); --i >= 0;) {
			tuple[0] = var0.getDomain().getValues().get(i);
			for (int j = var1.getDomain().getNbValues(); --j >= 0;) {
				tuple[1] = var1.getDomain().getValues().get(j);
				for (int k = var2.getDomain().getNbValues(); --k >= 0;) {
					tuple[2] = var2.getDomain().getValues().get(k);
					for (int l = var3.getDomain().getNbValues(); --l >= 0;) {
						tuple[3] = var3.getDomain().getValues().get(l);
						assertSame(initResults[i][j][k][l], constraint
								.evaluate(tuple));
					}
				}
			}
		}

		// System.out.println(problem.getRelationManager());
	}

	@Test
	public void loadMpsDiamonds() throws FileNotFoundException, ParseException,
			IOException, ScriptException {
		final Problem problem = Problem.load(this.getClass().getResource("mps-diamond.xml.bz2"));

		final List<Constraint> constraints = (List<Constraint>) problem
				.getConstraints();

		check(constraints.get(0), new int[][] { { 0, 1 }, { 1, 1 } });
		check(constraints.get(1), new int[][] { { 1, 1 }, { 1, 0 } });
		check(constraints.get(2), new int[][] { { 1, 1 }, { 0, 1 } });
		check(constraints.get(3), new int[][] { { 1, 1 }, { 0, 1 } });

		check(constraints.get(3).standardize(constraints.get(0).getScope()),
				new int[][] { { 1, 0 }, { 1, 1 } });

	}

	private static void check(Constraint constraint, int[][] matrix)
			throws ScriptException {
		for (int i = matrix.length; --i >= 0;) {
			for (int j = matrix[i].length; --j >= 0;) {
				assertSame(constraint + ", (" + i + "," + j + ")", constraint
						.evaluate(new Number[] { i, j }), matrix[i][j] > 0);
			}
		}
	}

}
