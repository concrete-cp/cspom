package cspom

import java.util.logging.Logger
import java.util.logging.Level
import java.io.PrintStream
import java.util.logging.LogRecord
import java.util.logging.Handler
trait Loggable {

  val logger: Logger = Logging.getLogger(this)

  def logInfo = logger.isLoggable(Level.INFO)

  def logFine = logger.isLoggable(Level.FINE)

  final def setLevel(level: Level) {
    logger.setLevel(level)
  }
}
final class MsLogHandler(val start: Long = System.currentTimeMillis(), val out: PrintStream = System.err) extends Handler {
  def close() {
    out.close();
  }

  def flush() {
    out.flush();
  }

  def publish(arg0: LogRecord) {
    if (isLoggable(arg0)) {
      out.println("%% [%8.3f] %7s : %s (%s.%s)".format(
        (arg0.getMillis() - start) / 1e3d, arg0.getLevel(),
        arg0.getMessage(), arg0.getSourceClassName(),
        arg0.getSourceMethodName()));
    }
  }

}
/**
 * Note: implementation taken from scalax.logging API
 */
object Logging {

  for (h <- Logger.getLogger("").getHandlers()) {
    Logger.getLogger("").removeHandler(h);
  }

  val handler = new MsLogHandler(System.currentTimeMillis());
  Logger.getLogger("").addHandler(handler);

  def setLevel(l: Level) {
    handler.setLevel(l)
    Logger.getLogger("").setLevel(l);
  }

  def loggerNameForClass(className: String) = {
    if (className endsWith "$") className.substring(0, className.length - 1)
    else className
  }

  def getLogger(logging: AnyRef) = Logger.getLogger(loggerNameForClass(logging.getClass.getName))

}
