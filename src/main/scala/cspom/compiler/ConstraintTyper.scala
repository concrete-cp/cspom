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
  val function: String,
  val arguments: CSPOMType*) {
  def fullScope = result +: arguments
}

final class ConstraintTyper(signatures: Seq[ConstraintSignature]) extends ConstraintCompiler {
  type A = Unit
  private val signMap = signatures.groupBy(_.function).toMap

  private def isCompatible(e: CSPOMExpression, t: CSPOMType) =
    e.cspomType == CSPOMFree || t.generalizes(e.cspomType)

  private def matchSignature(constraint: CSPOMConstraint): Seq[ConstraintSignature] = {
    signMap.getOrElse(constraint.function, Nil).filter { candidate =>
      (constraint.fullScope zip candidate.fullScope).forall(t => isCompatible(t._1, t._2))
    }
  }

  def matcher(constraint: CSPOMConstraint, problem: CSPOM) =
    Some(())

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

  private def enforce(variable: CSPOMExpression, problem: CSPOM, et: CSPOMType): Delta =
    variable match {
      case v: CSPOMVariable =>
        et match {
          case CSPOMInt => replaceVar(v, IntVariable.free(v.name, v.params), problem)
          case CSPOMBool => replaceVar(v, new BoolVariable(v.name, v.params), problem)
          case _ => Delta()
        }

      case _ => Delta()
    }

  private def enforce(constraint: CSPOMConstraint, problem: CSPOM, signatures: Seq[ConstraintSignature]): Delta = {
    //println(signatures)
    val t = signatures.map(_.fullScope).transpose
    //println(t)

    val toEnforce = (for ((arg, sign) <- constraint.fullScope zip t)
      yield (arg, enforcedType(arg.cspomType, sign))).groupBy(_._1)

    var delta = Delta()
    for ((arg, Seq(e)) <- toEnforce) {
      delta ++= enforce(arg, problem, e._2)
    }
    delta

  }

  def compile(constraint: CSPOMConstraint, problem: CSPOM, data: Unit) = {
    val signature = matchSignature(constraint)
    require(signature.nonEmpty,
      s"Could not identify a signature for $constraint (candidates: ${signMap.getOrElse(constraint.function, "None")})")

    enforce(constraint, problem, signature)
  }
}