package cspom
package xcsp

import cspom.CSPOM._
import cspom.extension.{MDD, MDDNode}
import cspom.util.IntervalsArithmetic.Arithmetics
import cspom.util.{Finite, IntInterval, MinInf, PlusInf}
import cspom.variable._
import org.xcsp.parser.XEnums._
import org.xcsp.parser.XNodeExpr.{XNodeLeaf, XNodeParent}
import org.xcsp.parser.{XCallbacks2, XConstants, XNodeExpr, XParser}
import org.xcsp.parser.XParser.{Condition, ConditionVal, ConditionVar}
import org.xcsp.parser.XVariables.{TypeVar, XVar, XVarInteger}

import scala.collection.mutable.{HashMap, LinkedHashMap}

//import scala.language.implicitConversions

class XCSP3Callbacks extends XCallbacks2 {

  val cspom: CSPOM = new CSPOM()

  private val declaredVariables = new LinkedHashMap[XVarInteger, CSPOMVariable[Int]]()

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

  override def endObjectives(): Unit = {
    val goal = cspom.goal
      .getOrElse(WithParam(CSPOMGoal.Satisfy))
      .withParam("variables" -> declaredVariables.map(_._1.id))
    cspom.setGoal(goal)
  }

  override def buildVarInteger(x: XVarInteger, lb: Int, ub: Int): Unit = {
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

  private def buildVar(x: XVarInteger, v: CSPOMVariable[Int]): Unit = {
    declaredVariables(x) = v
    cspom.nameExpression(v, x.id)
  }

  override def buildVarInteger(x: XVarInteger, values: Array[Int]): Unit = {
    buildVar(x, IntVariable.ofSeq(values))
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    buildCtrPrimitiveCSPOM(cspom(x), op, k)
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, opa: TypeArithmeticOperator,
                                 y: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    buildCtrPrimitiveCSPOM(cspom(x), opa, cspom(y), op, k)
  }

  /* Build Variables */

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
      case GT => cspom.ctr('gt)(x, k)
      case GE => cspom.ctr('ge)(x, k)
    }
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, opa: TypeArithmeticOperator,
                                 y: XVarInteger, op: TypeConditionOperatorRel, k: XVarInteger): Unit = {
    buildCtrPrimitiveCSPOM(cspom(x), opa, cspom(y), op, cspom(k))
  }

  /* Build constraints: intension */

  override def buildCtrIntension(id: String, scope: Array[XVarInteger], syntaxTreeRoot: XNodeParent) {

    def extract(node: XNodeExpr): SimpleExpression[_] = {
      node match {
        case l: XNodeLeaf =>
          l.value match {
            case l: java.lang.Long if l.toLong.isValidInt => CSPOMConstant(l.toInt)
            case v: XVarInteger => cspom(v)
          }
        case p: XNodeParent =>
          cspom.defineFree { x => constraint(x, p) }
      }
    }

    def typeSymbol(t: TypeExpr): Symbol = {
      Symbol(t.toString.toLowerCase)
    }

    def constraint(result: CSPOMExpression[_], p: XNodeParent): CSPOMConstraint[_] = {
      CSPOMConstraint(result)(typeSymbol(p.getType))(p.sons.toSeq.map(extract): _*)
    }

    cspom.ctr(constraint(CSPOMConstant(true), syntaxTreeRoot))
  }

  override def buildCtrExtension(id: String, list: Array[XVarInteger], tuples: Array[Array[Int]], positive: Boolean,
                                 flags: java.util.Set[TypeFlag]): Unit = {
    require(!flags.contains(TypeFlag.STARRED_TUPLES), "Starred tuples are not supported")

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

  override def buildCtrExtension(id: String, x: XVarInteger, values: Array[Int], positive: Boolean,
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
  }

  override def buildCtrMDD(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]]): Unit = {
    val nodeNames = transitions.groupBy(x => x(0))

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

  override def buildCtrAllDifferent(id: String, x: Array[XVarInteger]): Unit = {
    cspom.ctr('alldifferent)(cspom(x): _*)
  }

  /* Build constraints : extension */

  override def buildCtrAllEqual(id: String, list: Array[XVarInteger]): Unit = {
    cspom.ctr('eq)(cspom(list): _*)
  }

  override def buildCtrOrdered(id: String, list: Array[XVarInteger], operator: TypeOperator): Unit = {
    cspom.ctr(CSPOMConstraint('ordered)(cspom(list): _*) withParam "mode" -> operator.name)
  }

  /* Language constraints */

  override def buildCtrLex(id: String, list: Array[Array[XVarInteger]], operator: TypeOperator): Unit = {
    cspom.ctr(CSPOMConstraint('lex)(list.toSeq.map(cspom): _*) withParam "mode" -> operator.name)
  }

  override def buildCtrLexMatrix(id: String, list: Array[Array[XVarInteger]], operator: TypeOperator): Unit = {
    cspom.ctr(CSPOMConstraint('lexmatrix)(CSPOMSeq(list.map(cspomSeq): _*)) withParam "mode" -> operator.name)
  }

  /* Comparison constraints */

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
      case o => throw new AssertionError(s"Sum condition $o is not supported")
    }

    cspom.ctr {
      CSPOMConstraint('sum)(CSPOM.constantSeq(ks2), cspom(vars), CSPOMConstant(constant)) withParam ("mode" -> mode)
    }
  }

  override def buildCtrAtLeast(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    cspom.ctr {
      CSPOMConstraint('atLeast)(k, value, cspom(list))
    }
  }

  override def buildCtrAtMost(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    cspom.ctr {
      CSPOMConstraint('atMost)(k, value, cspom(list))
    }
  }

  private def cspom(x: Array[XVarInteger]): IndexedSeq[CSPOMVariable[Int]] = {
    x.map(declaredVariables)
  }

  /* Counting and summing constraints */

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    buildCtrExactly(list, value, k)
  }

  private def buildCtrExactly(list: Array[XVarInteger], value: Int, k: SimpleExpression[Int]): Unit = {
    cspom.ctr {
      CSPOMConstraint(k)('count)(value, cspom(list))
    }
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: XVarInteger): Unit = {
    buildCtrExactly(list, value, cspom(k))
  }

  private def cspom(x: XVarInteger): CSPOMVariable[Int] = declaredVariables(x)

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], condition: Condition) {
    val r = cspom.defineFree { v => CSPOMConstraint(v)('max)(cspom(list): _*) }
    implementCondition(r, condition)
  }

  private def implementCondition(r: CSPOMExpression[_], condition: Condition): Unit = {
    val v = condition match {
      case cv: ConditionVar => cspom(cv.x.asInstanceOf[XVarInteger])
      case cc: ConditionVal => CSPOMConstant(cc.value)
    }

    import TypeConditionOperator._

    condition.operator match {
      case LT => cspom.ctr('lt)(r, v)
      case LE => cspom.ctr('le)(r, v)
      case GE => cspom.ctr('ge)(r, v)
      case GT => cspom.ctr('gt)(r, v)
      case NE => cspom.ctr('ne)(r, v)
      case EQ => cspom.ctr('eq)(r, v)
      case IN => ???
      case NOTIN => ???
    }
  }

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], condition: Condition) {
    val r = cspom.defineFree { v => CSPOMConstraint(v)('min)(cspom(list): _*) }
    implementCondition(r, condition)
  }

  /* Connection constraints */

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: XVarInteger): Unit = {
    buildCtrElement(cspom(value), IntVariable.free(), cspomSeq(list))
  }

  private def cspomSeq(x: Array[XVarInteger]): CSPOMSeq[Int] = {
    CSPOM.seq2CSPOMSeq(cspom(x))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: Int) {
    buildCtrElement(value, IntVariable.free(), cspomSeq(list))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = {
    require(rank == TypeRank.ANY)

    buildCtrElement(cspom(value), cspom(index), cspomSeq(list, startIndex until startIndex + list.length))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: Int) {
    require(rank == TypeRank.ANY)

    buildCtrElement(value, cspom(index), cspomSeq(list, startIndex until startIndex + list.length))
  }

  private def cspomSeq(x: Array[XVarInteger], indices: Range): CSPOMSeq[Int] = {
    new CSPOMSeq(cspom(x), indices)
  }

  private def buildCtrElement(value: SimpleExpression[Int], index: SimpleExpression[Int], list: CSPOMSeq[Int]): Unit = {
    cspom.ctr {
      CSPOMConstraint(value)('element)(list, index)
    }
  }

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[XVarInteger],
                                 lengths: Array[Int],
                                 zeroIgnored: Boolean) {
    buildCtrNoOverlap(cspom(origins), CSPOM.constantSeq(lengths), zeroIgnored)
  }

  /* Packing and scheduling */

  private def buildCtrNoOverlap(origins: CSPOMSeq[Int], lengths: CSPOMSeq[Int], zeroIgnored: Boolean): Unit = {
    cspom.ctr {
      CSPOMConstraint('noOverlap)(origins, lengths) withParam "zeroIgnored" -> zeroIgnored
    }
  }

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[XVarInteger],
                                 lengths: Array[XVarInteger],
                                 zeroIgnored: Boolean) {
    buildCtrNoOverlap(cspom(origins), cspom(lengths), zeroIgnored)
  }

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[Array[XVarInteger]],
                                 lengths: Array[Array[Int]],
                                 zeroIgnored: Boolean) {
    unimplementedCase(id)
  }

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[Array[XVarInteger]],
                                 lengths: Array[Array[XVarInteger]],
                                 zeroIgnored: Boolean) {
    unimplementedCase(id)
  }

  /**
    * Default case
    */
  override def unimplementedCase(objects: Object*): Object = {
    throw new UnsupportedOperationException(objects.toString)
  }

  /* Elementary constraints */

  override def buildCtrInstantiation(id: String, list: Array[XVarInteger], values: Array[Int]) {
    implicit def problem = cspom

    for ((variable, value) <- (list, values).zipped) {
      cspom.ctr(cspom(variable) === constant(value))
    }

  }

  override def buildCtrClause(id: String, pos: Array[XVarInteger], neg: Array[XVarInteger]): Unit = {
    cspom.ctr('clause)(cspom(pos), cspom(neg))
  }

  /* Objectives */

  override def buildObjToMinimize(id: String, x: XVarInteger): Unit = {
    cspom.setGoal(CSPOMGoal.Minimize(cspom(x)))
  }

  override def buildObjToMaximize(id: String, x: XVarInteger): Unit = {
    cspom.setGoal(CSPOMGoal.Maximize(cspom(x)))
  }

  override def buildObjToMinimize(id: String, typ: TypeObjective, list: Array[XVarInteger]): Unit = {
    import TypeObjective._
    typ match {
      case MAXIMUM => cspom.setGoal(CSPOMGoal.Minimize(buildObjMax(list)))
      case MINIMUM => cspom.setGoal(CSPOMGoal.Minimize(buildObjMin(list)))
      case o => buildObjToMinimize(id, typ, list, Array.fill(list.length)(1))
    }

  }

  private def buildObjMax(list: Array[XVarInteger]) = {
    val vars = cspom(list)
    val obj = declare(IntVariable(union(vars)), "cspom-objective")
    cspom.ctr {
      CSPOMConstraint(obj)('max)(vars: _*)
    }
    obj
  }

  private def declare(e: CSPOMVariable[Int], name: String): CSPOMVariable[Int] = {
    val obj = cspom.nameExpression(e, "cspom-objective")
    declaredVariables(XVar.build("cspom-objective", TypeVar.integer, null).asInstanceOf[XVarInteger]) = obj
    obj
  }

  private def union(vars: Seq[SimpleExpression[Int]]) = {
    vars.map { v => IntExpression.span(v) }.reduce(_ span _)
  }

  private def buildObjMin(list: Array[XVarInteger]) = {
    val vars = cspom(list)
    val obj = declare(IntVariable(union(vars)), "cspom-objective")
    cspom.ctr {
      CSPOMConstraint(obj)('min)(vars: _*)
    }
    obj
  }

  override def buildObjToMinimize(id: String, typ: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): Unit = {
    import TypeObjective._
    typ match {
      case SUM => cspom.setGoal(CSPOMGoal.Minimize(buildObjSum(list, coefs, "le")))
      case o => throw new UnsupportedOperationException(s"Objective type $o is not implemented")
    }
  }

  private def buildObjSum(list: Array[XVarInteger], coefs: Array[Int], mode: String) = {
    val vars = cspom(list)

    val obj = declare(IntVariable(span(vars, coefs)), "cspom-objective")

    cspom.ctr {
      CSPOMConstraint('sum)(-1 +: coefs.toSeq, obj +: vars, 0) withParam ("mode" -> mode)
    }

    obj
  }

  private def span(vars: Seq[SimpleExpression[Int]], coefs: Array[Int]) = {
    vars.zip(coefs).map { case (v, k) => IntExpression.span(v) * Finite(k) }.reduce(_ + _)
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
      case SUM => cspom.setGoal(CSPOMGoal.Maximize(buildObjSum(list, coefs, "ge")))
      case o => throw new UnsupportedOperationException(s"Objective type $o is not implemented")

    }
  }

}