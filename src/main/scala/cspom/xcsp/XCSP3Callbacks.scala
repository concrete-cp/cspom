package cspom
package xcsp

import cspom.CSPOM._
import org.xcsp.parser.XCallbacks.{Implem, XCallbacksParameters}
import org.xcsp.parser.XParser
import org.xcsp.parser.entries.XVariables.XVarInteger


class XCSP3Callbacks extends XCSP3CallbacksObj
  with XCSP3CallbacksGeneric
  with XCSP3CallbacksLanguage
  with XCSP3CallbacksComparison
  with XCSP3CallbacksConnection
  with XCSP3CallbacksCountSum
  with XCSP3CallbacksPackSched {

  val cspom: CSPOM = new CSPOM()

  val implem = new Implem(this)

  implem.currParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_ARITY_LIMIT, Int.box(2))
  implem.currParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_PRIORITY, Boolean.box(true))
  implem.currParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_FORMULA_SIZE_MIN, Int.box(8))

  def loadInstance(parser: XParser): Unit = {
    beginInstance(parser.typeFramework)
    beginVariables(parser.vEntries)
    loadVariables(parser)
    endVariables()
    beginConstraints(parser.cEntries)
    loadConstraints(parser)
    endConstraints()
    beginObjectives(parser.oEntries, parser.typeCombination)
    loadObjectives(parser)
    endObjectives()
    // annotations
    endInstance()
  }


  override def buildCtrInstantiation(id: String, list: Array[XVarInteger], values: Array[Int]) {
    implicit def problem = cspom

    for ((variable, value) <- (list, values).zipped) {
      cspom.ctr(toCspom(variable) === constant(value))
    }

  }

  override def buildCtrClause(id: String, pos: Array[XVarInteger], neg: Array[XVarInteger]): Unit = {
    cspom.ctr('clause)(toCspom(pos), toCspom(neg))
  }

}