package cspom.compiler;

import cspom.CSPOM;

public interface ConstraintTransformer {

	CSPOM pattern();

	void generate(CSPOM sourceSubProblem);
}
