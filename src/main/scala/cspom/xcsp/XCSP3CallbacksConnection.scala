package cspom.xcsp

import cspom.CSPOMConstraint
import cspom.variable._
import org.xcsp.common.Condition
import org.xcsp.common.Condition.{ConditionRel, ConditionVal, ConditionVar}
import org.xcsp.common.Types.{TypeConditionOperatorRel, TypeRank}
import org.xcsp.parser.entries.XVariables.XVarInteger

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksConnection extends XCSP3CallbacksVars {

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], condition: Condition) {
    val r = cspom.defineFree { v => CSPOMConstraint(v)('max)(toCspom(list): _*) }
    implementCondition(r, condition)
  }

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], condition: Condition) {
    val r = cspom.defineFree { v => CSPOMConstraint(v)('min)(toCspom(list): _*) }
    implementCondition(r, condition)
  }

  private def implementCondition(r: CSPOMExpression[_], condition: Condition): Unit = {
    condition match {
      case condition: ConditionRel =>

        val v = condition match {
          case cv: ConditionVar => toCspom(cv.x.asInstanceOf[XVarInteger])
          case cc: ConditionVal => CSPOMConstant(cc.k)
        }

        import TypeConditionOperatorRel._

        condition.operator match {
          case LT => cspom.ctr('lt)(r, v)
          case LE => cspom.ctr('le)(r, v)
          case GE => cspom.ctr('ge)(r, v)
          case GT => cspom.ctr('gt)(r, v)
          case NE => cspom.ctr('ne)(r, v)
          case EQ => cspom.ctr('eq)(r, v)
          case e => unimplementedCase(e)
        }
      case c => unimplementedCase(c)
    }
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: XVarInteger): Unit = {
    buildCtrMember(toCspom(value), cspomSeq(list))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: Int) {
    buildCtrMember(CSPOMConstant(value), cspomSeq(list))
  }

  private def buildCtrMember(value: SimpleExpression[Int], list: CSPOMSeq[Int]): Unit = {
    cspom.ctr {
      CSPOMConstraint('member)(list, value)
    }
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = {
    require(rank == TypeRank.ANY)

    buildCtrElement(toCspom(value), toCspom(index), cspomSeq(list, startIndex until startIndex + list.length))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: Int) {
    require(rank == TypeRank.ANY)

    buildCtrElement(CSPOMConstant(value), toCspom(index), cspomSeq(list, startIndex until startIndex + list.length))
  }

  private def buildCtrElement(value: SimpleExpression[Int], index: SimpleExpression[Int], list: CSPOMSeq[Int]): Unit = {
    cspom.ctr {
      CSPOMConstraint(value)('element)(list, index)
    }
  }

  override def buildCtrElement(id: String, list: Array[Int], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = {
    require(rank == TypeRank.ANY)

    buildCtrElement(toCspom(value), toCspom(index), CSPOMSeq(list.map(CSPOMConstant(_)), startIndex until startIndex + list.length))

  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int): Unit = {
    cspom.ctr('channel)(
      CSPOMSeq(toCspom(list), startIndex until startIndex + list.length))
  }

  override def buildCtrChannel(id: String, list1: Array[XVarInteger], startIndex1: Int, list2: Array[XVarInteger], startIndex2: Int): Unit = {
    cspom.ctr('inverse)(
      CSPOMSeq(toCspom(list1), startIndex1 until startIndex1 + list1.length),
      CSPOMSeq(toCspom(list2), startIndex2 until startIndex2 + list2.length)
    )
  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int, value: XVarInteger): Unit = {
    cspom.ctr('channelBool)(
      CSPOMSeq(toCspom(list), startIndex until startIndex + list.length), toCspom(value))
  }


}
