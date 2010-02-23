package cspom.xcsp;


public final class XCSPParseException extends Exception {

    /**
     * 
     */
    private static final long serialVersionUID = -4074106911956475429L;

    private final int lineNumber;

    public XCSPParseException(String message) {
        this(message, -1);
    }

    public XCSPParseException(Throwable cause) {
        this(cause, -1);
    }

    public XCSPParseException(String message, Throwable cause) {
        this(message, cause, -1);
    }

    public XCSPParseException(String message, int lineNumber) {
        super(message);
        this.lineNumber = lineNumber;
    }

    public XCSPParseException(Throwable cause, int lineNumber) {
        super(cause);
        this.lineNumber = lineNumber;
    }

    public XCSPParseException(String message, Throwable cause, int lineNumber) {
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
     * @return An integer representing the line number, or -1 if none is
     *         available.
     */
    public int getLineNumber() {
        return this.lineNumber;
    }
}
