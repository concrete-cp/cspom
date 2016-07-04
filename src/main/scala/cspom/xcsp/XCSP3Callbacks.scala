package cspom.xcsp

import scala.annotation.varargs
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap

import org.xcsp.parser.XCallbacks2
import org.xcsp.parser.XConstants
import org.xcsp.parser.XEnums.TypeArithmeticOperator
import org.xcsp.parser.XEnums.TypeConditionOperator
import org.xcsp.parser.XEnums.TypeConditionOperatorRel
import org.xcsp.parser.XEnums.TypeFlag
import org.xcsp.parser.XEnums.TypeObjective
import org.xcsp.parser.XParser
import org.xcsp.parser.XParser.Condition
import org.xcsp.parser.XParser.ConditionVal
import org.xcsp.parser.XVariables.XVarInteger

import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.CSPOMGoal
import cspom.extension.MDD
import cspom.extension.MDDNode
import cspom.util.Finite
import cspom.util.IntInterval
import cspom.util.MinInf
import cspom.util.PlusInf
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression
import cspom.variable.IntExpression
import cspom.util.IntervalsArithmetic.Arithmetics
import org.xcsp.parser.XVariables.XVar
import org.xcsp.parser.XVariables.TypeVar
import org.xcsp.parser.XEnums.TypeRank
import org.xcsp.parser.XParser.ConditionVar

//import scala.language.implicitConversions

class XCSP3Callbacks extends XCallbacks2 {

  val cspom: CSPOM = new CSPOM()

  private val declaredVariables = new LinkedHashMap[XVarInteger, CSPOMVariable[Int]]()

  var goal: CSPOMGoal[Int] = CSPOMGoal.Satisfy

  private def cspom(x: XVarInteger): CSPOMVariable[Int] = declaredVariables(x)

  private def cspom(x: Array[XVarInteger]): Seq[CSPOMVariable[Int]] = {
    x.map { v: XVarInteger => declaredVariables(v) }
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
    cspom.setGoal(goal, Map("variables" -> declaredVariables.map(_._1.id)))
  }

  def buildVarInteger(x: XVarInteger, lb: Int, ub: Int): Unit = {
    val ilb = lb match {
      case XConstants.VAL_MINUS_INFINITY_INT => MinInf
      case l => Finite(lb)
    }

    val iub = ub match {
      case XConstants.VAL_PLUS_INFINITY_INT => PlusInf
      case u => Finite(ub)
    }

    buildVar(x, IntVariable(IntInterval(ilb, iub)))
  }

  def buildVarInteger(x: XVarInteger, values: Array[Int]): Unit = {
    buildVar(x, IntVariable.ofSeq(values))
  }

  private def buildVar(x: XVarInteger, v: CSPOMVariable[Int]): Unit = {
    declaredVariables(x) = v
    cspom.nameExpression(v, x.id)
  }

  def buildCtrExtension(id: String, list: Array[XVarInteger], tuples: Array[Array[Int]], positive: Boolean,
    flags: java.util.Set[TypeFlag]): Unit = {
    if (flags.contains(TypeFlag.STARRED_TUPLES)) {
      throw new UnsupportedOperationException("Starred tuples are not supported")
    }

    val relation = tuples.view.map(_.toList)
    val scope = list.toSeq.map(declaredVariables)

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
        case ADD => CSPOMConstraint(r)('add)(x, y)
        case SUB => CSPOMConstraint(r)('sub)(x, y)
        case MUL => CSPOMConstraint(r)('mul)(x, y)
        case DIV => CSPOMConstraint(r)('div)(x, y)
        case MOD => CSPOMConstraint(r)('mod)(x, y)
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

  private def span(vars: Seq[SimpleExpression[Int]], coefs: Array[Int]) = {
    vars.zip(coefs).map { case (v, k) => IntExpression.span(v) * Finite(k) }.reduce(_ + _)
  }

  private def buildObjSum(list: Array[XVarInteger], coefs: Array[Int], mode: String) = {
    val vars = cspom(list)

    val obj = cspom.nameExpression(IntVariable(span(vars, coefs)), "cspom-objective")

    cspom.ctr {
      CSPOMConstraint('sum)(-1 +: coefs.toSeq, obj +: vars, 0) withParam ("mode" -> mode)
    }

    declaredVariables(XVar.build("cspom-objective", TypeVar.integer, null).asInstanceOf[XVarInteger]) = obj

    obj
  }

  override def buildObjToMinimize(id: String, typ: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): Unit = {
    import TypeObjective._
    typ match {
      case SUM =>
        goal = CSPOMGoal.Minimize(buildObjSum(list, coefs, "le"))
      case _ => ???

    }
  }

  override def buildObjToMaximize(id: String, typ: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): Unit = {
    import TypeObjective._
    typ match {
      case SUM =>
        goal = CSPOMGoal.Maximize(buildObjSum(list, coefs, "ge"))
      case _ => ???

    }
  }

  override def buildCtrRegular(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]], startState: String, finalStates: Array[String]): Unit = {

    val states = transitions.flatMap { case Array(i: String, _, o: String) => Seq(i, o) }.distinct.zipWithIndex.toMap
    val dfa = transitions
      .view
      .map {
        case Array(i: String, v: Any, o: String) =>
          (states(i), v.asInstanceOf[Any]) -> states(o)
      }
      .toMap

    cspom.ctr(
      CSPOMConstraint('regular)(cspom(list), states(startState), finalStates.toSeq.map(states)) withParam ("dfa" -> dfa))
    //unimplementedCase(id);
  }

  override def buildCtrMDD(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]]): Unit = {
    //    val nodeNames = new HashMap[String, Map[Int, String]]().withDefaultValue(Map())
    //
    //    for (Array(source: String, value: Integer, child: String) <- transitions) {
    //      nodeNames(source) += (value.toInt -> child)
    //    }

    val nodeNames = transitions.groupBy { x => x(0) }

    val nodes = new HashMap[AnyRef, MDD[Any]]()
    nodes("nodeT") = MDD.leaf

    def buildMDD(node: AnyRef): MDD[Any] = {
      nodes.getOrElseUpdate(node, new MDDNode(
        nodeNames(node).view
          .map {
            case Array(_, value, target) =>
              value -> buildMDD(target)
          }
          .toMap))
    }

    cspom.ctr(
      cspom(list).asInstanceOf[Seq[CSPOMVariable[Any]]] in buildMDD("root"))

  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    buildCtrSum(id, list, Array.fill(list.length)(1), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[Int], condition: Condition) {
    import TypeConditionOperator._

    val (vars, ks1, k) = condition match {
      case cond: ConditionVal => (list, coeffs, cond.k)
      case cond: ConditionVar => (cond.x.asInstanceOf[XVarInteger] +: list, -1 +: coeffs, 0)
    }

    val (ks2, constant, mode) = condition.operator match {
      case LT => (ks1, k, "lt")
      case LE => (ks1, k, "le")
      case GE => (ks1.map(-_), -k, "le")
      case GT => (ks1.map(-_), -k, "lt")
      case NE => (ks1, k, "ne")
      case EQ => (ks1, k, "eq")
    }

    cspom.ctr {
      CSPOMConstraint('sum)(CSPOM.constantSeq(ks2), cspom(vars), CSPOMConstant(constant)) withParam ("mode" -> mode)
    }
  }

  private def buildCtrExactly(list: Array[XVarInteger], value: Int, k: SimpleExpression[Int]): Unit = {
    cspom.ctr {
      CSPOMConstraint(k)('count)(value, cspom(list))
    }
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    buildCtrExactly(list, value, k)
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: XVarInteger): Unit = {
    buildCtrExactly(list, value, cspom(k))
  }

  private def buildCtrElement(value: SimpleExpression[Int], index: SimpleExpression[Int], list: CSPOMSeq[Int]): Unit = {
    cspom.ctr {
      CSPOMConstraint(value)('element)(list, index)
    }
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: XVarInteger): Unit = {
    buildCtrElement(cspom(value), IntVariable.free(), cspom(list))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: Int) {
    buildCtrElement(value, IntVariable.free(), cspom(list))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = {
    require(rank == TypeRank.ANY)

    buildCtrElement(cspom(value), cspom(index), new CSPOMSeq(list.map(cspom), startIndex until startIndex + list.length))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: Int) {
    require(rank == TypeRank.ANY)

    buildCtrElement(value, cspom(index), new CSPOMSeq(list.map(cspom), startIndex until startIndex + list.length))
  }

  override def unimplementedCase(objects: Object*): Object = {
    throw new UnsupportedOperationException(objects.toString)
  }
}