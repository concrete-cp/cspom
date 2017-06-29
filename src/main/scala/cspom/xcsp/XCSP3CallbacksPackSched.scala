package cspom.xcsp

import cspom.variable.{CSPOMConstant, CSPOMExpression, CSPOMSeq}
import cspom.{CSPOM, CSPOMConstraint}
import org.xcsp.common.Condition
import org.xcsp.common.Condition.{ConditionVal, ConditionVar}
import org.xcsp.common.Types.TypeConditionOperatorRel
import org.xcsp.parser.entries.XVariables.XVarInteger

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksPackSched extends XCSP3CallbacksVars {


  override def buildCtrNoOverlap(id: String,
                                 origins: Array[XVarInteger],
                                 lengths: Array[Int],
                                 zeroIgnored: Boolean) {
    buildCtrNoOverlap(cspomSeq(origins), CSPOM.constantSeq(lengths), zeroIgnored)
  }

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[XVarInteger],
                                 lengths: Array[XVarInteger],
                                 zeroIgnored: Boolean) {
    buildCtrNoOverlap(cspomSeq(origins), cspomSeq(lengths), zeroIgnored)
  }

  private def buildCtrNoOverlap(origins: CSPOMSeq[Int], lengths: CSPOMSeq[Int], zeroIgnored: Boolean): Unit = {
    cspom.ctr {
      CSPOMConstraint('noOverlap)(origins, lengths) withParam "zeroIgnored" -> zeroIgnored
    }
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], heights: Array[Int], condition: Condition): Unit = {
    cumulative(cspomSeq(origins), cspomSeq(lengths), cspomSeq(heights), condition)
  }

  private def cumulative(s: CSPOMSeq[Int], d: CSPOMSeq[Int], r: CSPOMSeq[Int], condition: Condition): Unit = {
    val (operator: TypeConditionOperatorRel, operand) = condition match {
      case c: ConditionVar => (c.operator, toCspom(c.involvedVar.asInstanceOf[XVarInteger]))
      case c: ConditionVal => (c.operator, CSPOMConstant(Math.toIntExact(c.k)))
      case c: Any => throw new UnsupportedOperationException(c.toString)
    }

    require(operator == TypeConditionOperatorRel.LE)

    cspom.ctr('cumulative)(s, d, r, operand)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], heights: Array[XVarInteger], condition: Condition): Unit = {
    cumulative(cspomSeq(origins), cspomSeq(lengths), cspomSeq(heights), condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = {
    cumulative(cspomSeq(origins), cspomSeq(lengths), cspomSeq(heights), condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = {
    cumulative(cspomSeq(origins), cspomSeq(lengths), cspomSeq(heights), condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = {
    endsCtr(toCspom(origins), lengths.map(CSPOMConstant(_)), toCspom(ends))
    cumulative(cspomSeq(origins), cspomSeq(lengths), cspomSeq(heights), condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = {
    endsCtr(toCspom(origins), lengths.map(CSPOMConstant(_)), toCspom(ends))
    cumulative(cspomSeq(origins), cspomSeq(lengths), cspomSeq(heights), condition)

  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = {
    endsCtr(toCspom(origins), toCspom(lengths), toCspom(ends))
    cumulative(cspomSeq(origins), cspomSeq(lengths), cspomSeq(heights), condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = {
    endsCtr(toCspom(origins), toCspom(lengths), toCspom(ends))
    cumulative(cspomSeq(origins), cspomSeq(lengths), cspomSeq(heights), condition)
  }

  private def endsCtr(s: Seq[CSPOMExpression[Int]], d: Seq[CSPOMExpression[Int]], e: Seq[CSPOMExpression[Int]]) = {
    (s, d, e).zipped.foreach { (s, d, e) =>
      cspom.ctr(CSPOMConstraint(e)('add)(s, d))
    }
  }

}
