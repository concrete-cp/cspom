package cspom.xcsp;

import java.io.InputStream

import scala.util.Try

import cspom.CSPOM
import org.xcsp.parser.XParser
import org.w3c.dom.Document

/**
 * This class implements an XCSP 3.0 parser.
 *
 * @author vion
 */
final object XCSP3Parser extends CSPOM.Parser {

  /**
   * Append the XCSP data provided by the InputStream to the given CSPOM
   * problem.
   *
   * @param is
   *            The source of the XCSP data
   * @throws CSPParseException
   *             Thrown if is contains invalid data
   * @throws IOException
   *             Thrown if the data could not be read
   */
  def apply(is: InputStream): Try[CSPOM] =
    Try {
      new XParser(is)
    }
      .flatMap(apply)

  def apply(doc: Document): Try[CSPOM] =
    Try {
      new XParser(doc)
    }
      .flatMap(apply)

  def apply(parser: XParser): Try[CSPOM] = Try {
    val callbacks = new XCSP3Callbacks()
    callbacks.loadInstance(parser)
    callbacks.cspom
  }

}

