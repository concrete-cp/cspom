package cspom.xcsp

import org.xcsp.parser.XParser
import cspom.CSPOM
import org.xcsp.parser.XCallbacks2
import org.xcsp.parser.XVariables.XVarInteger
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import org.xcsp.parser.XVariables.XVar
import cspom.util.IntInterval
import org.xcsp.parser.XConstants
import cspom.util.MinInf
import cspom.util.Finite
import cspom.util.PlusInf
import cspom.util.Interval
import org.xcsp.parser.XEnums.TypeFlag
import cspom.extension.MDD
import cspom.variable.CSPOMSeq
import CSPOM._
import cspom.CSPOMConstraint
import cspom.CSPOMGoal
import scala.annotation.varargs
import org.xcsp.parser.XEnums.TypeConditionOperator
import org.xcsp.parser.XEnums.TypeConditionOperatorRel
import org.xcsp.parser.XEnums.TypeArithmeticOperator
import cspom.variable.CSPOMConstant
import cspom.variable.SimpleExpression
import org.xcsp.parser.XEnums.TypeObjective

//import scala.language.implicitConversions

class XCSP3Callbacks extends XCallbacks2 {

  val cspom: CSPOM = new CSPOM()

  var declaredVariables: Map[XVarInteger, CSPOMVariable[Int]] = Map()

  var goal: CSPOMGoal[Int] = CSPOMGoal.Satisfy

  def cspom(x: XVarInteger): CSPOMVariable[Int] = declaredVariables(x)

  def cspom(x: Array[XVarInteger]): CSPOMSeq[Int] = {
    x.view.map { v: XVarInteger => declaredVariables(v) }
  }

  def loadInstance(parser: XParser): Unit = {
    beginInstance(parser.typeFramework);
    beginVariables(parser.vEntries);
    loadVariables(parser);
    endVariables();
    beginConstraints(parser.cEntries);
    loadConstraints(parser);
    endConstraints();
    beginObjectives(parser.oEntries, parser.typeCombination);
    loadObjectives(parser.oEntries);
    endObjectives();
    // annotations
    endInstance();
  }

  def endObjectives(): Unit = {
    cspom.setGoal(goal, Map("variables" -> declaredVariables.keys.map(_.id)))
  }

  def buildVarInteger(x: XVarInteger, lb: Int, ub: Int): Unit = {
    val ilb = lb match {
      case XConstants.VAL_MINUS_INFINITY_INT => MinInf
      case l                                 => Finite(lb)
    }

    val iub = ub match {
      case XConstants.VAL_PLUS_INFINITY_INT => PlusInf
      case u                                => Finite(ub)
    }

    buildVar(x, IntVariable(IntInterval(ilb, iub)))
  }

  def buildVarInteger(x: XVarInteger, values: Array[Int]): Unit = {
    buildVar(x, IntVariable.ofSeq(values))
  }

  private def buildVar(x: XVarInteger, v: CSPOMVariable[Int]): Unit = {
    declaredVariables += (x -> v)
    cspom.nameExpression(v, x.id)
  }

  def buildCtrExtension(id: String, list: Array[XVarInteger], tuples: Array[Array[Int]], positive: Boolean,
                        flags: java.util.Set[TypeFlag]): Unit = {
    if (flags.contains(TypeFlag.STARRED_TUPLES)) {
      throw new UnsupportedOperationException("Starred tuples are not supported")
    }

    val relation = tuples.view.map(_.toList)
    val scope = list.view.map(declaredVariables)

    cspom.ctr {
      if (positive) {
        scope in relation
      } else {
        scope notIn relation
      }
    }

  }

  def buildCtrExtension(id: String, x: XVarInteger, values: Array[Int], positive: Boolean,
                        flags: java.util.Set[TypeFlag]): Unit = {
    val relation = values.map(List(_))
    val scope = Seq(cspom(x))

    cspom.ctr {
      if (positive) {
        scope in relation
      } else {
        scope notIn relation
      }
    }
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    buildCtrPrimitiveCSPOM(id, cspom(x), op, k)
  }

  private def buildCtrPrimitiveCSPOM(id: String, x: SimpleExpression[Int], op: TypeConditionOperatorRel, k: SimpleExpression[Int]): Unit = {
    import TypeConditionOperatorRel._
    op match {
      case LT => cspom.ctr(CSPOMConstraint('lt)(x, k))
      case LE => cspom.ctr(CSPOMConstraint('le)(x, k))
      case EQ => cspom.ctr(CSPOMConstraint('eq)(x, k))
      case NE => cspom.ctr(CSPOMConstraint('ne)(x, k))
      case GT => cspom.ctr(CSPOMConstraint('gt)(x, k))
      case GE => cspom.ctr(CSPOMConstraint('ge)(x, k))
    }
  }

  def buildCtrPrimitive(id: String, x: XVarInteger, opa: TypeArithmeticOperator,
                        y: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    buildCtrPrimitiveCSPOM(id, cspom(x), opa, cspom(y), op, k)
  }

  def buildCtrPrimitive(id: String, x: XVarInteger, opa: TypeArithmeticOperator,
                        y: XVarInteger, op: TypeConditionOperatorRel, k: XVarInteger): Unit = {
    buildCtrPrimitiveCSPOM(id, cspom(x), opa, cspom(y), op, cspom(k))
  }

  private def buildCtrPrimitiveCSPOM(id: String, x: SimpleExpression[Int], opa: TypeArithmeticOperator,
                                     y: SimpleExpression[Int], op: TypeConditionOperatorRel, k: SimpleExpression[Int]): Unit = {
    import TypeArithmeticOperator._
    val aux = cspom.defineInt { r =>
      opa match {
        case ADD  => CSPOMConstraint(r)('add)(x, y)
        case SUB  => CSPOMConstraint(r)('sub)(x, y)
        case MUL  => CSPOMConstraint(r)('mul)(x, y)
        case DIV  => CSPOMConstraint(r)('div)(x, y)
        case MOD  => CSPOMConstraint(r)('mod)(x, y)
        case DIST => CSPOMConstraint(r)('absdiff)(x, y)
      }
    }

    buildCtrPrimitiveCSPOM(id + "_aux_ctr", aux, op, k)
  }

  override def buildCtrAllDifferent(id: String, x: Array[XVarInteger]): Unit = {
    cspom.ctr(CSPOMConstraint('alldifferent)(cspom(x): _*))
  }

  override def buildObjToMinimize(id: String, x: XVarInteger): Unit = {
    goal = CSPOMGoal.Minimize(cspom(x))
  }

  override def buildObjToMaximize(id: String, x: XVarInteger): Unit = {
    goal = CSPOMGoal.Maximize(cspom(x))
  }

  override def buildObjToMinimize(id: String, typ: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): Unit = {
    import TypeObjective._
    typ match {
      case SUM =>
        val obj = cspom.defineInt { x =>
          CSPOMConstraint('sum)(-1 +: coefs.toSeq, x +: cspom(list), 0) withParam ("mode" -> "le")
        }
        buildCtrPrimitiveCSPOM(id, obj, TypeConditionOperatorRel.LT, Int.MaxValue)
        goal = CSPOMGoal.Minimize(obj)
      case _ => ???

    }
  }

  override def buildObjToMaximize(id: String, typ: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): Unit = {
    import TypeObjective._
    typ match {
      case SUM =>
        val obj = cspom.defineInt { x =>
          CSPOMConstraint('sum)(-1 +: coefs.toSeq, x +: cspom(list), 0) withParam ("mode" -> "ge")
        }
        buildCtrPrimitiveCSPOM(id, obj, TypeConditionOperatorRel.GT, Int.MinValue)
        goal = CSPOMGoal.Maximize(obj)
      case _ => ???

    }
  }

  override def unimplementedCase(objects: Object*): Object = {
    throw new UnsupportedOperationException(objects.toString)
  }
}