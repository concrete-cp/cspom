package cspom.compiler;

import java.util.ArrayList;
import java.util.Collection;

import cspom.CSPOM;
import cspom.variable.CSPOMVariable;

public class ProblemCompiler {
	private final CSPOM problem;

	public ProblemCompiler(final CSPOM problem) {
		this.problem = problem;

	}

	public void compile() {
		deReify();
		removeSingles();
	}

	public void deReify() {

	}

	public void removeSingles() {

		final Collection<CSPOMVariable> varToRemove = new ArrayList<CSPOMVariable>();

		for (CSPOMVariable v : problem.getVariables()) {
			if (v.getConstraints().isEmpty()) {
				varToRemove.add(v);
			}
		}

		for (CSPOMVariable v : varToRemove) {
			problem.removeVariable(v);
		}

	}

}
