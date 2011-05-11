package cspom;

/**
 * Exception used to report an error while parsing an XCSP file.
 *
 * @author vion
 *
 */
final class CSPParseException(
  message: String = null,
  cause: Throwable = null,
  val lineNumber: Int = -1) extends Exception(
  if (message == null) {
    if (cause == null) null
    else cause.getMessage
  } else message, cause) {}
      
