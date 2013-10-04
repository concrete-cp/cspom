package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.CSPOMBool
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMFree
import cspom.variable.CSPOMInt
import cspom.variable.CSPOMType
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable

final case class ConstraintSignature(
  val result: CSPOMType,
  val function: Symbol,
  val arguments: CSPOMType*) {
  def fullScope = result +: arguments
}

final class ConstraintTyper(signatures: Seq[ConstraintSignature]) extends ConstraintCompiler {
  type A = Map[CSPOMExpression, CSPOMType]
  private val signMap = signatures.groupBy(_.function).toMap

  private def isCompatible(e: CSPOMExpression, t: CSPOMType) =
    e.cspomType == CSPOMFree || t.generalizes(e.cspomType)

  private def matchSignature(constraint: CSPOMConstraint): Seq[ConstraintSignature] = {
    signMap.getOrElse(constraint.function, Nil).filter { candidate =>
      (constraint.fullScope zip candidate.fullScope).forall(t => isCompatible(t._1, t._2))
    }
  }

  private def enforcedType(typ: CSPOMType, candidates: Seq[CSPOMType]): CSPOMType = {
    if (typ == CSPOMFree) {
      candidates.filter(_ != CSPOMFree).distinct match {
        case Seq(single) => single
        case _ => CSPOMFree
      }
    } else {
      CSPOMFree
    }

  }

  def mtch(constraint: CSPOMConstraint, problem: CSPOM) = {
    val signature = matchSignature(constraint)
    require(signature.nonEmpty,
      s"Could not identify a signature for $constraint (candidates: ${signMap.getOrElse(constraint.function, "None")})")
    val transposed = signature.map(_.fullScope).transpose
    //println(t)

    val toEnforce = (constraint.fullScope zip transposed).map {
      case (arg, sign) => arg -> enforcedType(arg.cspomType, sign)
    }.groupBy(_._1).collect {
      case (arg, Seq((_, sign))) => arg -> sign
    }

    if (toEnforce.nonEmpty) {
      Some(toEnforce)
    } else {
      None
    }
  }

  private def enforce(variable: CSPOMExpression, problem: CSPOM, et: CSPOMType): Delta =
    variable match {
      case v: CSPOMVariable =>
        et match {
          case CSPOMInt => replaceVars(Seq(v), IntVariable.free(v.name, v.params), problem)
          case CSPOMBool => replaceVars(Seq(v), new BoolVariable(v.name, v.params), problem)
          case _ => Delta()
        }

      case _ => Delta()
    }

  def compile(constraint: CSPOMConstraint, problem: CSPOM, data: A) =
    data.foldLeft(Delta()) {
      case (acc, (arg, sign)) => acc ++ enforce(arg, problem, sign)
    }

}