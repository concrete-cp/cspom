package cspom.constraint;

public interface EvaluableConstraint<T> extends CSPOMConstraint {
	boolean evaluate(final T[] tuple);
}
