package cspom.xcsp

import com.typesafe.scalalogging.LazyLogging
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}
import org.xcsp.common.Condition
import org.xcsp.common.Condition.{ConditionIntvl, ConditionVal, ConditionVar}
import org.xcsp.common.Types.TypeConditionOperatorRel
import org.xcsp.common.predicates.XNodeParent
import org.xcsp.parser.entries.XVariables.XVarInteger

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksCountSum extends XCSP3CallbacksVars with XCSP3CallbacksGeneric with LazyLogging {

  override def buildCtrSum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    buildCtrSum(id, list, Array.fill(list.length)(1), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[Int], condition: Condition): Unit = {
    for (ss <- toSumSig(toCspom(list), CSPOM.constantSeq(coeffs), condition)) {
      buildSum(ss)
    }
  }

  case class SumSig(variables: Seq[CSPOMExpression[Int]], coeffs: Seq[CSPOMExpression[Int]], condition: TypeConditionOperatorRel, k: Int) {
    def stringCondition: String = condition.toString.toLowerCase

  }

  private def toSumSig(vars: Seq[CSPOMExpression[Int]], coeffs: Seq[CSPOMExpression[Int]], condition: Condition): Seq[SumSig] = {
    condition match {
      case cond: ConditionVal => Seq(SumSig(vars, coeffs, cond.operator, Math.toIntExact(cond.k)))
      case cond: ConditionVar => Seq(SumSig(toCspom(cond.x.asInstanceOf[XVarInteger]) +: vars, CSPOMConstant(-1) +: coeffs, cond.operator, 0))
      case cond: ConditionIntvl =>
        toSumSig(vars, coeffs, new ConditionVal(TypeConditionOperatorRel.GE, cond.min)) ++
          toSumSig(vars, coeffs, new ConditionVal(TypeConditionOperatorRel.LE, cond.max))
      case o =>
        throw new UnsupportedOperationException(s"Sum condition $o is not supported")
    }
  }

  protected def buildSum(s: SumSig): Unit = {

    val mode = s.stringCondition

    cspom.ctr {
      CSPOMConstraint('sum)(CSPOMSeq(s.coeffs: _*), CSPOMSeq(s.variables: _*), CSPOMConstant(s.k)) withParam ("mode" -> mode)
    }
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[XVarInteger], condition: Condition): Unit = {
    for (ss <- toSumSig(toCspom(list), toCspom(coeffs), condition)) {
      buildSum(ss)
    }
  }

  override def buildCtrSum(id: String, trees: Array[XNodeParent[XVarInteger]], coeffs: Array[Int], condition: Condition): Unit = {
    val variables = trees.toSeq.map { node =>
      cspom.defineInt(x => intensionConstraint(x, node))
    }
    for (ss <- toSumSig(variables, CSPOM.constantSeq(coeffs), condition)) {
      buildSum(ss)
    }
  }

  override def buildCtrAtLeast(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    cspom.ctr {
      CSPOMConstraint('atLeast)(CSPOMConstant(k), CSPOMConstant(value), cspomSeq(list))
    }
  }

  override def buildCtrAtMost(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    cspom.ctr {
      CSPOMConstraint('atMost)(CSPOMConstant(k), CSPOMConstant(value), cspomSeq(list))
    }
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    buildCtrExactly(list, value, CSPOMConstant(k))
  }

  private def buildCtrExactly(list: Array[XVarInteger], value: Int, k: SimpleExpression[Int]): Unit = {
    cspom.ctr {
      CSPOMConstraint(k)('occurrence)(CSPOMConstant(value), cspomSeq(list))
    }
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: XVarInteger): Unit = {
    buildCtrExactly(list, value, toCspom(k))
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[XVarInteger]): Unit = {
    buildCardExact(list, closed, CSPOM.constantSeq(values), cspomSeq(occurs))
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[XVarInteger]): Unit = {
    buildCardExact(list, closed, cspomSeq(values), cspomSeq(occurs))
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[Int]): Unit = {
    buildCardExact(list, closed, CSPOM.constantSeq(values), CSPOM.constantSeq(occurs))
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[Int]): Unit = {
    buildCardExact(list, closed, cspomSeq(values), CSPOM.constantSeq(occurs))
  }

  private def buildCardExact(list: Array[XVarInteger], closed: Boolean, values: CSPOMSeq[Int], occurs: CSPOMSeq[Int]): Unit = {
    cspom.ctr('gccExact)(cspomSeq(list), CSPOMConstant(closed), values, occurs)
  }


  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occursMin: Array[Int], occursMax: Array[Int]): Unit = {
    buildCardMinMax(list, closed, CSPOM.constantSeq(values), occursMin, occursMax)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occursMin: Array[Int], occursMax: Array[Int]): Unit = {
    buildCardMinMax(list, closed, cspomSeq(values), occursMin, occursMax)
  }

  private def buildCardMinMax(list: Array[XVarInteger], closed: Boolean, values: CSPOMSeq[Int], occursMin: Seq[Int], occursMax: Seq[Int]): Unit = {
    cspom.ctr('gccMinMax)(cspomSeq(list), CSPOMConstant(closed), values, CSPOM.constantSeq(occursMin), CSPOM.constantSeq(occursMax))
  }

  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[Int], condition: Condition): Unit = {
    val count = values.map(i => cspom.defineInt(v => CSPOMConstraint(v)('occurrence)(CSPOMConstant(i), cspomSeq(list))))
    for (ss <- toSumSig(count, Seq.fill(count.length)(CSPOMConstant(1)), condition)) {
      buildSum(ss)
    }
  }

  override def buildCtrCount(s: String, list: Array[XVarInteger], values: Array[XVarInteger], condition: Condition): Unit = {
    val count = values.map(i => cspom.defineInt(v => CSPOMConstraint(v)('occurrence)(toCspom(i), cspomSeq(list))))
    for (ss <- toSumSig(count, Seq.fill(count.length)(CSPOMConstant(1)), condition)) {
      buildSum(ss)
    }
  }

  override final def buildCtrNValues(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    buildCtrNValuesExcept(id, list, Array(), condition)
  }

  override final def buildCtrNValuesExcept(id: String, list: Array[XVarInteger], except: Array[Int], condition: Condition): Unit = {
    val r = cspom.defineInt(r => CSPOMConstraint(r)('nvalues)(toCspom(list): _*) withParam ("except" -> except.toSeq))
    for (ss <- toSumSig(Seq(r), Seq(CSPOMConstant(1)), condition)) buildSum(ss)
  }

}
