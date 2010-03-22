package cspom;

/**
 * Exception used to report an error while parsing an XCSP file.
 * 
 * @author vion
 * 
 */
public final class CSPParseException extends Exception {

    /**
     * 
     */
    private static final long serialVersionUID = -4074106911956475429L;

    /**
     * Line where the error occurred, if available (-1 if not).
     */
    private final int lineNumber;

    /**
     * Standard constructor with message.
     * 
     * @param message
     *            a message to be attached to the exception.
     */
    public CSPParseException(final String message) {
        this(message, -1);
    }

    /**
     * Standard constructor with attached Throwable cause.
     * 
     * @param cause
     *            the Throwable that caused this exception to be thrown.
     */
    public CSPParseException(final Throwable cause) {
        this(cause, -1);
    }

    /**
     * Standard constructor with both attached message and Throwable cause.
     * 
     * @param message
     *            a message to be attached to the exception.
     * @param cause
     *            the Throwable that caused this exception to be thrown.
     */
    public CSPParseException(final String message, final Throwable cause) {
        this(message, cause, -1);
    }

    /**
     * Constructor with attached message and line number.
     * 
     * @param message
     *            a message to be attached to the exception.
     * @param lineNumber
     *            a line number in the parsed file where the exception occurred.
     */
    public CSPParseException(final String message, final int lineNumber) {
        super(message);
        this.lineNumber = lineNumber;
    }

    /**
     * Standard constructor with attached Throwable cause and line number.
     * 
     * @param cause
     *            the Throwable that caused this exception to be thrown.
     * @param lineNumber
     *            a line number in the parsed file where the exception occurred.
     */
    public CSPParseException(final Throwable cause, final int lineNumber) {
        super(cause);
        this.lineNumber = lineNumber;
    }

    /**
     * Standard constructor with both attached message, a Throwable cause and
     * line number.
     * 
     * @param message
     *            a message to be attached to the exception.
     * @param cause
     *            the Throwable that caused this exception to be thrown.
     * @param lineNumber
     *            a line number in the parsed file where the exception occurred.
     */
    public CSPParseException(final String message, final Throwable cause,
            final int lineNumber) {
        super(message, cause);
        this.lineNumber = lineNumber;
    }

    /**
     * The line number of the end of the text where the exception occurred.
     * 
     * <p>
     * The first line is line 1.
     * </p>
     * 
     * @return an integer representing the line number, or -1 if none is
     *         available.
     */
    public int getLineNumber() {
        return this.lineNumber;
    }
}
