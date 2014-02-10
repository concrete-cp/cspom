package cspom

import java.util.logging.Logger
import java.util.logging.Level
trait Loggable {

  val logger: Logger = Logging.getLogger(this)

  def logInfo = logger.isLoggable(Level.INFO)

  def logFine = logger.isLoggable(Level.FINE)

  final def setLevel(level: Level) {
    logger.setLevel(level)
  }
}

/**
 * Note: implementation taken from scalax.logging API
 */
object Logging {
  def loggerNameForClass(className: String) = {
    if (className endsWith "$") className.substring(0, className.length - 1)
    else className
  }

  def getLogger(logging: AnyRef) = Logger.getLogger(loggerNameForClass(logging.getClass.getName))

}
