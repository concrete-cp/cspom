package cspom.xcsp

import cspom.util.{Finite, IntInterval, MinInf, PlusInf}
import cspom.variable._
import cspom.{CSPOM, CSPOMGoal, WithParam}
import org.xcsp.common.Constants
import org.xcsp.parser.XCallbacks2
import org.xcsp.parser.entries.XVariables.XVarInteger

import scala.collection.mutable.LinkedHashMap

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksVars extends XCallbacks2 {
  private val declaredVariables = new LinkedHashMap[XVarInteger, CSPOMVariable[Int]]()

  def cspom: CSPOM

  override def buildVarInteger(x: XVarInteger, lb: Int, ub: Int): Unit = {
    val ilb = lb match {
      case Constants.VAL_MINUS_INFINITY_INT => MinInf
      case l => Finite(l)
    }

    val iub = ub match {
      case Constants.VAL_PLUS_INFINITY_INT => PlusInf
      case u => Finite(u)
    }

    buildVar(x, IntVariable(IntInterval(ilb, iub)))
  }

  private def buildVar(x: XVarInteger, v: CSPOMVariable[Int]): Unit = {
    declaredVariables(x) = v
    cspom.nameExpression(v, x.id())
  }

  override def buildVarInteger(x: XVarInteger, values: Array[Int]): Unit = {
    buildVar(x, IntVariable.ofSeq(values))
  }

  def toCspom(x: XVarInteger): CSPOMVariable[Int] = declaredVariables(x)

  override def endObjectives(): Unit = {
    val goal = cspom.goal
      .getOrElse(WithParam(CSPOMGoal.Satisfy))
      .withParam("variables" -> declaredVariables.map(_._1.id()))
    cspom.setGoal(goal)
  }

  def cspomSeq(x: Array[XVarInteger]): CSPOMSeq[Int] = {
    CSPOM.seq2CSPOMSeq(toCspom(x))
  }

  def toCspom(x: Array[XVarInteger]): IndexedSeq[CSPOMVariable[Int]] = {
    x.map(declaredVariables)
  }

  def cspomSeq(x: Array[XVarInteger], indices: Range): CSPOMSeq[Int] = {
    new CSPOMSeq(toCspom(x), indices)
  }

  /**
    * Default case
    */
  override def unimplementedCase(objects: Object*): Object = {
    throw new UnsupportedOperationException(objects.toString)
  }

}
