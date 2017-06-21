package cspom.xcsp

import com.typesafe.scalalogging.LazyLogging
import cspom.variable.{CSPOMConstant, CSPOMExpression, CSPOMSeq, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}
import org.xcsp.common.Condition
import org.xcsp.common.Condition.{ConditionVal, ConditionVar}
import org.xcsp.common.Types.TypeConditionOperatorRel
import org.xcsp.parser.entries.XVariables.XVarInteger

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksCountSum extends XCSP3CallbacksVars with LazyLogging {

  override def buildCtrSum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    buildCtrSum(id, list, Array.fill(list.length)(1), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[Int], condition: Condition): Unit = {
    val (vars, ks1, k, op) = manageCondition(list, CSPOM.constantSeq(coeffs), condition)

    buildSum(toCspom(vars), ks1, op, k)
  }

  private def manageCondition(list: Array[XVarInteger], coeffs: Seq[CSPOMExpression[Int]], condition: Condition):
  (Array[XVarInteger], Seq[CSPOMExpression[Int]], Int, TypeConditionOperatorRel) = {
    condition match {
      case cond: ConditionVal => (list, coeffs, Math.toIntExact(cond.k), cond.operator)
      case cond: ConditionVar => (cond.x.asInstanceOf[XVarInteger] +: list, CSPOMConstant(-1) +: coeffs, 0, cond.operator)
      case o =>
        throw new UnsupportedOperationException(s"Sum condition $o is not supported")
    }
  }

  protected def buildSum(vars: Seq[CSPOMExpression[Int]], ks1: Seq[CSPOMExpression[Int]], operator: TypeConditionOperatorRel, k: Int): Unit = {
    import TypeConditionOperatorRel._
    val (ks2: Seq[CSPOMExpression[Int]], constant: Int, mode: String) = operator match {
      case LT => (ks1, k, "lt")
      case LE => (ks1, k, "le")
      case GE => (ks1.map { case CSPOMConstant(k) => CSPOMConstant(-k) }, -k, "le")
      case GT => (ks1.map { case CSPOMConstant(k) => CSPOMConstant(-k) }, -k, "lt")
      case NE => (ks1, k, "ne")
      case EQ => (ks1, k, "eq")
      case o => unimplementedCase(s"Sum operator $o is not supported")
    }

    cspom.ctr {
      CSPOMConstraint('sum)(CSPOMSeq(ks2: _*), CSPOMSeq(vars: _*), CSPOMConstant(constant)) withParam ("mode" -> mode)
    }
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[XVarInteger], condition: Condition): Unit = {
    val (vars, ks1, k, op) = manageCondition(list, toCspom(coeffs), condition)

    val mode = op.name().toLowerCase()

    cspom.ctr {
      CSPOMConstraint('sum)(CSPOMSeq(ks1: _*), cspomSeq(vars), CSPOMConstant(k)) withParam ("mode" -> mode)
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


}
