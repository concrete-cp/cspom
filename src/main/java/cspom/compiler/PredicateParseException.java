package cspom.compiler;

public final class PredicateParseException extends Exception {

    /**
	 * 
	 */
    private static final long serialVersionUID = -2680104562146014148L;

    public PredicateParseException(final String message) {
        super(message);
    }

    public PredicateParseException(final Throwable cause) {
        super(cause);
    }

    public PredicateParseException(final String message, final Throwable cause) {
        super(message, cause);
    }

}
