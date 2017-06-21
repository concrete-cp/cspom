package cspom.xcsp

import cspom.util.{Finite, Infinitable, Interval}
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint, CSPOMGoal}
import org.xcsp.common.Types.{TypeConditionOperatorRel, TypeObjective}
import org.xcsp.parser.XCallbacks2
import org.xcsp.parser.entries.XVariables.XVarInteger
import cspom.util.IntervalsArithmetic._

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksObj extends XCallbacks2 with XCSP3CallbacksVars with XCSP3CallbacksCountSum {


  override def buildObjToMinimize(id: String, x: XVarInteger): Unit = {
    cspom.setGoal(CSPOMGoal.Minimize(toCspom(x)))
  }

  override def buildObjToMinimize(id: String, typ: TypeObjective, list: Array[XVarInteger]): Unit = {
    import TypeObjective._
    typ match {
      case MAXIMUM => cspom.setGoal(CSPOMGoal.Minimize(buildObjMax(list)))
      case MINIMUM => cspom.setGoal(CSPOMGoal.Minimize(buildObjMin(list)))
      case o => buildObjToMinimize(id, typ, list, Array.fill(list.length)(1))
    }

  }

  override def buildObjToMinimize(id: String, typ: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): Unit = {
    import TypeObjective._
    typ match {
      case SUM => cspom.setGoal(CSPOMGoal.Minimize(buildObjSum(list, coefs)))
      case o => throw new UnsupportedOperationException(s"Objective type $o is not implemented")
    }
  }

  override def buildObjToMaximize(id: String, x: XVarInteger): Unit = {
    cspom.setGoal(CSPOMGoal.Maximize(toCspom(x)))
  }

  override def buildObjToMaximize(id: String, typ: TypeObjective, list: Array[XVarInteger]): Unit = {
    import TypeObjective._
    typ match {
      case MAXIMUM => cspom.setGoal(CSPOMGoal.Maximize(buildObjMax(list)))
      case MINIMUM => cspom.setGoal(CSPOMGoal.Maximize(buildObjMin(list)))
      case o => buildObjToMaximize(id, o, list, Array.fill(list.length)(1))
    }

  }

  override def buildObjToMaximize(id: String, typ: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): Unit = {
    import TypeObjective._
    typ match {
      case SUM => cspom.setGoal(CSPOMGoal.Maximize(buildObjSum(list, coefs)))
      case o => throw new UnsupportedOperationException(s"Objective type $o is not implemented")

    }
  }

  private def buildObjSum(list: Array[XVarInteger], coefs: Array[Int]) = {
    val vars = toCspom(list)

    val obj = declareObj(IntVariable(span(vars, coefs)))

    val constants = -1 +: coefs
    val variables = obj +: vars

    buildSum(variables, CSPOM.constantSeq(constants), TypeConditionOperatorRel.EQ, 0)

    obj
  }

  private def span(vars: Seq[SimpleExpression[Int]], coefs: Array[Int]): Interval[Infinitable] = {
    vars.zip(coefs).map { case (v, k) => IntExpression.span(v) * Finite(k) }.reduce(_ + _)
  }

  private def buildObjMax(list: Array[XVarInteger]) = {
    val vars = toCspom(list)
    val obj = declareObj(IntVariable(union(vars)))
    cspom.ctr {
      CSPOMConstraint(obj)('max)(vars: _*)
    }
    obj
  }

  private def buildObjMin(list: Array[XVarInteger]) = {
    val vars = toCspom(list)
    val obj = declareObj(IntVariable(union(vars)))
    cspom.ctr {
      CSPOMConstraint(obj)('min)(vars: _*)
    }
    obj
  }

  private def union(vars: Seq[SimpleExpression[Int]]) = {
    vars.map { v => IntExpression.span(v) }.reduce(_ span _)
  }

  private def declareObj(e: CSPOMVariable[Int]): CSPOMVariable[Int] = {
    cspom.nameExpression(e, "cspom-objective")
    // declaredVariables(XVar.build("cspom-objective", TypeVar.integer, null).asInstanceOf[XVarInteger]) = obj
    // obj
  }


}
