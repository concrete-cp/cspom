package cspom
package xcsp

import cspom.CSPOM._
import cspom.extension.MDDRelation
import cspom.util.IntervalsArithmetic.Arithmetics
import cspom.util.{Finite, IntInterval, MinInf, PlusInf}
import cspom.variable._
import mdd.{JavaMap, MDD, MDDLeaf}
import org.xcsp.common.Condition.{ConditionRel, ConditionVal, ConditionVar}
import org.xcsp.common.Types._
import org.xcsp.common.predicates.{XNode, XNodeLeaf, XNodeParent}
import org.xcsp.common.{Condition, Constants}
import org.xcsp.parser.XCallbacks.Implem
import org.xcsp.parser.entries.XVariables.XVarInteger
import org.xcsp.parser.{XCallbacks2, XParser}

import scala.collection.mutable.LinkedHashMap

//import scala.language.implicitConversions

class XCSP3Callbacks extends XCallbacks2 {

  val cspom: CSPOM = new CSPOM()

  val implem = new Implem(this)

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
    loadObjectives(parser);
    endObjectives();
    // annotations
    endInstance();
  }

  override def endObjectives(): Unit = {
    val goal = cspom.goal
      .getOrElse(WithParam(CSPOMGoal.Satisfy))
      .withParam("variables" -> declaredVariables.map(_._1.id()))
    cspom.setGoal(goal)
  }

  /* Build Variables */
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

  override def buildVarInteger(x: XVarInteger, values: Array[Int]): Unit = {
    buildVar(x, IntVariable.ofSeq(values))
  }

  private def buildVar(x: XVarInteger, v: CSPOMVariable[Int]): Unit = {
    declaredVariables(x) = v
    cspom.nameExpression(v, x.id())
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    buildCtrPrimitiveCSPOM(toCspom(x), op, k)
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, aop: TypeArithmeticOperator,
                                 p: Int, op: TypeConditionOperatorRel, y: XVarInteger): Unit = {
    buildCtrPrimitiveCSPOM(toCspom(x), aop, p, op, toCspom(y))
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, opa: TypeArithmeticOperator,
                                 y: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    buildCtrPrimitiveCSPOM(toCspom(x), opa, toCspom(y), op, k)
  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, op: TypeConditionOperatorSet, min: Int, max: Int): Unit = {
    import TypeConditionOperatorSet._
    op match {
      case IN =>
        cspom.ctr('in)(toCspom(x), min to max)

      case NOTIN =>
        cspom.ctr(CSPOMConstraint(CSPOMConstant(false))('in)(toCspom(x), min to max))
    }

  }

  override def buildCtrPrimitive(id: String, x: XVarInteger, opa: TypeArithmeticOperator,
                                 y: XVarInteger, op: TypeConditionOperatorRel, k: XVarInteger): Unit = {
    buildCtrPrimitiveCSPOM(toCspom(x), opa, toCspom(y), op, toCspom(k))
  }

  private def toCspom(x: XVarInteger): CSPOMVariable[Int] = declaredVariables(x)

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

  /* Build constraints: intension */

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
    if (flags.contains(TypeFlag.STARRED_TUPLES)) {
      throw new UnsupportedOperationException("Starred tuples are not supported")
    }

    val relation = MDDRelation(tuples.view.map(_.toSeq))
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
    val relation = MDDRelation(values.map(Seq(_)))
    val scope = Seq(toCspom(x))

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
      CSPOMConstraint('regular)(toCspom(list), states(startState), finalStates.toSeq.map(states)) withParam ("dfa" -> dfa))
  }

  override def buildCtrMDD(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]]): Unit = {
    val nodeNames = transitions.groupBy(x => x(0))

    val nodes = new JavaMap[String, MDD]()
    nodes.put("nodeT", MDDLeaf)

    def buildMDD(node: String): MDD = {
      nodes.getOrElseUpdate(node, {
        val newVal = MDD(
          nodeNames(node).view
            .map {
              case Array(_, value: java.lang.Long, target: String) =>
                java.lang.Math.toIntExact(value) -> buildMDD(target)
            }
        )
        nodes.put(node, newVal)
        newVal
      })
    }

    cspom.ctr(
      toCspom(list).asInstanceOf[Seq[CSPOMVariable[Int]]] in new MDDRelation(buildMDD("root")))

  }

  private def toCspom(x: Array[XVarInteger]): IndexedSeq[CSPOMVariable[Int]] = {
    x.map(declaredVariables)
  }

  override def buildCtrAllDifferent(id: String, x: Array[XVarInteger]): Unit = {
    cspom.ctr('alldifferent)(toCspom(x): _*)
  }

  /* Build constraints : extension */

  override def buildCtrAllDifferentList(id: String, lists: Array[Array[XVarInteger]]): Unit = {
    for (Array(a1, a2) <- lists.combinations(2)) {
      cspom.ctr('nevec)(toCspom(a1), toCspom(a2))
    }
  }

  override def buildCtrAllEqual(id: String, list: Array[XVarInteger]): Unit = {
    cspom.ctr('eq)(toCspom(list): _*)
  }

  /* Language constraints */

  override def buildCtrOrdered(id: String, list: Array[XVarInteger], operator: TypeOperatorRel): Unit = {
    cspom.ctr(CSPOMConstraint('ordered)(toCspom(list): _*) withParam "mode" -> operator.name)
  }

  override def buildCtrLex(id: String, list: Array[Array[XVarInteger]], operator: TypeOperatorRel): Unit = {
    cspom.ctr(CSPOMConstraint('lex)(list.toSeq.map(toCspom): _*) withParam "mode" -> operator.name)
  }

  /* Comparison constraints */

  override def buildCtrLexMatrix(id: String, list: Array[Array[XVarInteger]], operator: TypeOperatorRel): Unit = {
    cspom.ctr(CSPOMConstraint('lexmatrix)(CSPOMSeq(list.map(cspomSeq): _*)) withParam "mode" -> operator.name)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    buildCtrSum(id, list, Array.fill(list.length)(1), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[Int], condition: Condition) {

    val (vars: Array[XVarInteger], ks1: Array[Int], k: Long, operator: TypeConditionOperatorRel) = condition match {
      case cond: ConditionVal => (list, coeffs, cond.k, cond.operator)
      case cond: ConditionVar => (cond.x.asInstanceOf[XVarInteger] +: list, -1 +: coeffs, 0, cond.operator)
      case o => unimplementedCase(s"Sum condition $o is not supported")
    }

    import TypeConditionOperatorRel._
    val (ks2: Array[Int], constant: Long, mode: String) = operator match {
      case LT => (ks1, k, "lt")
      case LE => (ks1, k, "le")
      case GE => (ks1.map(-_), -k, "le")
      case GT => (ks1.map(-_), -k, "lt")
      case NE => (ks1, k, "ne")
      case EQ => (ks1, k, "eq")
      case o => unimplementedCase(s"Sum operator $o is not supported")
    }

    cspom.ctr {
      CSPOMConstraint('sum)(CSPOM.constantSeq(ks2), toCspom(vars), CSPOMConstant(constant)) withParam ("mode" -> mode)
    }
  }

  /* Counting and summing constraints */
  override def buildCtrAtLeast(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    cspom.ctr {
      CSPOMConstraint('atLeast)(k, value, toCspom(list))
    }
  }

  override def buildCtrAtMost(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    cspom.ctr {
      CSPOMConstraint('atMost)(k, value, toCspom(list))
    }
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    buildCtrExactly(list, value, k)
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: XVarInteger): Unit = {
    buildCtrExactly(list, value, toCspom(k))
  }

  private def buildCtrExactly(list: Array[XVarInteger], value: Int, k: SimpleExpression[Int]): Unit = {
    cspom.ctr {
      CSPOMConstraint(k)('count)(value, toCspom(list))
    }
  }

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], condition: Condition) {
    val r = cspom.defineFree { v => CSPOMConstraint(v)('max)(toCspom(list): _*) }
    implementCondition(r, condition)
  }

  private def implementCondition(r: CSPOMExpression[_], condition: Condition): Unit = {
    condition match {
      case condition: ConditionRel =>

        val v = condition match {
          case cv: ConditionVar => toCspom(cv.x.asInstanceOf[XVarInteger])
          case cc: ConditionVal => CSPOMConstant(cc.value)
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

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], condition: Condition) {
    val r = cspom.defineFree { v => CSPOMConstraint(v)('min)(toCspom(list): _*) }
    implementCondition(r, condition)
  }

  /* Connection constraints */

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: XVarInteger): Unit = {
    buildCtrElement(toCspom(value), IntVariable.free(), cspomSeq(list))
  }

  private def cspomSeq(x: Array[XVarInteger]): CSPOMSeq[Int] = {
    CSPOM.seq2CSPOMSeq(toCspom(x))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: Int) {
    buildCtrElement(value, IntVariable.free(), cspomSeq(list))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = {
    require(rank == TypeRank.ANY)

    buildCtrElement(toCspom(value), toCspom(index), cspomSeq(list, startIndex until startIndex + list.length))
  }

  private def buildCtrElement(value: SimpleExpression[Int], index: SimpleExpression[Int], list: CSPOMSeq[Int]): Unit = {
    cspom.ctr {
      CSPOMConstraint(value)('element)(list, index)
    }
  }

  private def cspomSeq(x: Array[XVarInteger], indices: Range): CSPOMSeq[Int] = {
    new CSPOMSeq(toCspom(x), indices)
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: Int) {
    require(rank == TypeRank.ANY)

    buildCtrElement(value, toCspom(index), cspomSeq(list, startIndex until startIndex + list.length))
  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int): Unit = {
    cspom.ctr('channel)(
      new CSPOMSeq(toCspom(list), startIndex until startIndex + list.length))
  }

  override def buildCtrChannel(id: String, list1: Array[XVarInteger], startIndex1: Int, list2: Array[XVarInteger], startIndex2: Int): Unit = {
    cspom.ctr('inverse)(
      new CSPOMSeq(toCspom(list1), startIndex1 until startIndex1 + list1.length),
      new CSPOMSeq(toCspom(list2), startIndex2 until startIndex2 + list2.length)
    )
  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int, value: XVarInteger): Unit = {
    unimplementedCase(id)
  }

  /**
    * Default case
    */
  override def unimplementedCase(objects: Object*): Object = {
    throw new UnsupportedOperationException(objects.toString)
  }

  /* Packing and scheduling */

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[XVarInteger],
                                 lengths: Array[Int],
                                 zeroIgnored: Boolean) {
    buildCtrNoOverlap(toCspom(origins), CSPOM.constantSeq(lengths), zeroIgnored)
  }

  private def buildCtrNoOverlap(origins: CSPOMSeq[Int], lengths: CSPOMSeq[Int], zeroIgnored: Boolean): Unit = {
    cspom.ctr {
      CSPOMConstraint('noOverlap)(origins, lengths) withParam "zeroIgnored" -> zeroIgnored
    }
  }

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[XVarInteger],
                                 lengths: Array[XVarInteger],
                                 zeroIgnored: Boolean) {
    buildCtrNoOverlap(toCspom(origins), toCspom(lengths), zeroIgnored)
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

  /* Elementary constraints */

  override def buildCtrInstantiation(id: String, list: Array[XVarInteger], values: Array[Int]) {
    implicit def problem = cspom

    for ((variable, value) <- (list, values).zipped) {
      cspom.ctr(toCspom(variable) === constant(value))
    }

  }

  override def buildCtrClause(id: String, pos: Array[XVarInteger], neg: Array[XVarInteger]): Unit = {
    cspom.ctr('clause)(toCspom(pos), toCspom(neg))
  }

  /* Objectives */

  override def buildObjToMinimize(id: String, x: XVarInteger): Unit = {
    cspom.setGoal(CSPOMGoal.Minimize(toCspom(x)))
  }

  override def buildObjToMaximize(id: String, x: XVarInteger): Unit = {
    cspom.setGoal(CSPOMGoal.Maximize(toCspom(x)))
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
      case SUM => cspom.setGoal(CSPOMGoal.Minimize(buildObjSum(list, coefs, "le")))
      case o => throw new UnsupportedOperationException(s"Objective type $o is not implemented")
    }
  }

  private def buildObjSum(list: Array[XVarInteger], coefs: Array[Int], mode: String) = {
    val vars = toCspom(list)

    val obj = declareObj(IntVariable(span(vars, coefs)))

    cspom.ctr {
      CSPOMConstraint('sum)(-1 +: coefs.toSeq, obj +: vars, 0) withParam ("mode" -> mode)
    }

    obj
  }

  private def span(vars: Seq[SimpleExpression[Int]], coefs: Array[Int]) = {
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

  private def declareObj(e: CSPOMVariable[Int]): CSPOMVariable[Int] = {
    cspom.nameExpression(e, "cspom-objective")
    // declaredVariables(XVar.build("cspom-objective", TypeVar.integer, null).asInstanceOf[XVarInteger]) = obj
    // obj
  }

  private def union(vars: Seq[SimpleExpression[Int]]) = {
    vars.map { v => IntExpression.span(v) }.reduce(_ span _)
  }

  private def buildObjMin(list: Array[XVarInteger]) = {
    val vars = toCspom(list)
    val obj = declareObj(IntVariable(union(vars)))
    cspom.ctr {
      CSPOMConstraint(obj)('min)(vars: _*)
    }
    obj
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