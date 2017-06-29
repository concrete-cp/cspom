package cspom.xcsp

import cspom.extension.MDDRelation
import cspom.variable.{CSPOMConstant, CSPOMExpression, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}
import mdd.{IdMap, Star, Starrable, ValueStar}
import org.xcsp.common.Constants
import org.xcsp.common.Types._
import org.xcsp.common.predicates.{XNode, XNodeLeaf, XNodeParent}
import org.xcsp.parser.entries.XDomains.XDomBasic
import org.xcsp.parser.entries.XValues.IntegerEntity
import org.xcsp.parser.entries.XVariables.XVarInteger

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksGeneric extends XCSP3CallbacksVars {

  val cache = new IdMap[Array[Array[Int]], MDDRelation]()

  override def buildCtrPrimitive(id: String, x: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    buildCtrPrimitiveCSPOM(toCspom(x), op, CSPOMConstant(k))
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, aop: TypeArithmeticOperator,
                                 p: Int, op: TypeConditionOperatorRel, y: XVarInteger): Unit = {
    buildCtrPrimitiveCSPOM(toCspom(x), aop, CSPOMConstant(p), op, toCspom(y))
  }

  private def buildCtrPrimitiveCSPOM(x: SimpleExpression[Int], opa: TypeArithmeticOperator,
                                     y: SimpleExpression[Int], op: TypeConditionOperatorRel, k: SimpleExpression[Int]): Unit = {
    val aux = cspom.defineInt { r =>
      CSPOMConstraint(r)(Symbol(opa.toString.toLowerCase))(x, y)
    }

    buildCtrPrimitiveCSPOM(aux, op, k)
  }

  private def buildCtrPrimitiveCSPOM(x: SimpleExpression[Int], op: TypeConditionOperatorRel, k: SimpleExpression[Int]): Unit = {
    import TypeConditionOperatorRel._
    op match {
      case LT => cspom.ctr('lt)(x, k)
      case LE => cspom.ctr('le)(x, k)
      case EQ => cspom.ctr('eq)(x, k)
      case NE => cspom.ctr('ne)(x, k)
      case GT => cspom.ctr('lt)(k, x)
      case GE => cspom.ctr('le)(k, x)
    }
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, opa: TypeArithmeticOperator,
                                 y: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    if (opa == TypeArithmeticOperator.SUB && k == 0) {
      // Optimizes the optimizer…
      buildCtrPrimitiveCSPOM(toCspom(x), op, toCspom(y))
    } else {
      buildCtrPrimitiveCSPOM(toCspom(x), opa, toCspom(y), op, CSPOMConstant(k))
    }
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, op: TypeConditionOperatorSet, min: Int, max: Int): Unit = {
    buildCtrPrimitive(id, x, op, Array.range(min, max + 1))
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, op: TypeConditionOperatorSet, array: Array[Int]): Unit = {
    import TypeConditionOperatorSet._
    op match {
      case IN =>
        cspom.ctr('in)(toCspom(x), CSPOM.constantSeq(array))

      case NOTIN =>
        cspom.ctr(CSPOMConstraint(CSPOMConstant(false))('in)(toCspom(x), CSPOM.constantSeq(array)))
    }
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, opa: TypeArithmeticOperator,
                                 y: XVarInteger, op: TypeConditionOperatorRel, k: XVarInteger): Unit = {
    buildCtrPrimitiveCSPOM(toCspom(x), opa, toCspom(y), op, toCspom(k))
  }

  /* Build constraints: intension */

  override def buildCtrPrimitive(id: String, x: XVarInteger, aop: TypeArithmeticOperator, p: Int, op: TypeConditionOperatorRel, k: Int): Unit = {
    buildCtrPrimitiveCSPOM(toCspom(x), aop, CSPOMConstant(p), op, CSPOMConstant(k))
  }

  override def buildCtrIntension(id: String, scope: Array[XVarInteger], syntaxTreeRoot: XNodeParent[XVarInteger]) {

    def extract(node: XNode[XVarInteger]): SimpleExpression[_] = {
      node match {
        case l: XNodeLeaf[XVarInteger] =>
          l.value match {
            case l: java.lang.Long if l.toLong.isValidInt => CSPOMConstant(l.toInt)
            case v: XVarInteger => toCspom(v)
          }
        case p: XNodeParent[XVarInteger] =>
          cspom.defineFree { x => constraint(x, p) }
      }
    }

    def typeSymbol(t: TypeExpr): Symbol = {
      Symbol(t.toString.toLowerCase)
    }

    def constraint(result: CSPOMExpression[_], p: XNodeParent[XVarInteger]): CSPOMConstraint[_] = {
      CSPOMConstraint(result)(typeSymbol(p.getType))(p.sons.toSeq.map(extract): _*)
    }

    cspom.ctr(constraint(CSPOMConstant(true), syntaxTreeRoot))
  }

  override def buildCtrExtension(id: String, list: Array[XVarInteger], tuples: Array[Array[Int]], positive: Boolean,
                                 flags: java.util.Set[TypeFlag]): Unit = {

    val relation = cache.getOrElseUpdate(tuples, {

      if (flags.contains(TypeFlag.STARRED_TUPLES)) {

        val doms = list.map { l =>
          val dom = l.dom.asInstanceOf[XDomBasic].values.asInstanceOf[Array[IntegerEntity]]
          IntegerEntity.toIntArray(dom, Integer.MAX_VALUE).toSeq
        }

        val starredTuples: Traversable[IndexedSeq[Starrable]] = tuples.map { t =>
          IndexedSeq.tabulate(list.length) { i =>
            t(i) match {
              case Constants.STAR_INT => Star
              case v => ValueStar(v)
            }
          }
        }

        MDDRelation.fromStarred(starredTuples, doms).reduce()

      } else {
        MDDRelation(tuples.view.map(_.toIndexedSeq))
      }
    })

    val scope = CSPOM.IntSeqOperations(list.toSeq.map(toCspom))

    cspom.ctr {
      if (positive) {
        scope in relation
      } else {
        scope notIn relation
      }
    }

  }

  override def buildCtrExtension(id: String, x: XVarInteger, values: Array[Int], positive: Boolean,
                                 flags: java.util.Set[TypeFlag]): Unit = {
    val relation = MDDRelation(values.map(IndexedSeq(_)))
    val scope = CSPOM.IntSeqOperations(Seq(toCspom(x)))

    cspom.ctr {
      if (positive) {
        scope in relation
      } else {
        scope notIn relation
      }
    }
  }

}
