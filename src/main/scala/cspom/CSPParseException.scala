package cspom;

/**
 * Exception used to report an error while parsing an XCSP file.
 *
 * @author vion
 *
 */
final class CSPParseException(
  message: String,
  cause: Throwable = null,
  val lineNumber: Int = -1)
    extends Exception(
      message + (if (lineNumber < 0) "" else " at line " + lineNumber),
      cause)