package cspom.compiler;

public final class PredicateParseException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = -2680104562146014148L;

	public PredicateParseException(String message) {
		super(message);
	}

	public PredicateParseException(Throwable cause) {
		super(cause);
	}

	public PredicateParseException(String message, Throwable cause) {
		super(message, cause);
	}

}
