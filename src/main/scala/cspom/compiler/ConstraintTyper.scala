package cspom.compiler

import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.CSPOMConstraint
import cspom.variable.CSPOMType

case class ConstraintSignature(val result: CSPOMType, val function: String, val arguments: CSPOMType*)

object ConstraintTyper {

  def matchSignature(constraint: CSPOMConstraint, signatures: Map[String, Seq[ConstraintSignature]]): Option[ConstraintSignature] = {
    for (
      candidates <- signatures.get(constraint.function);
      found <- candidates.find { candidate =>
        constraint.result.cspomType.isCompatible(candidate.result) &&
          (constraint.arguments zip candidate.arguments).forall { t =>
            t._1.cspomType.isCompatible(t._2)
          }
      }
    ) yield found
  }

  def typer(problem: CSPOM, signatures: Seq[ConstraintSignature]): CSPOM = {
    val signMap = signatures.groupBy(_.function).toMap

    val variables = collection.mutable.Set[CSPOMVariable]()
    variables ++= problem.variables
    val constraints = collection.mutable.Set[CSPOMConstraint]()
    constraints ++= problem.constraints

    val queue = new collection.mutable.Queue[CSPOMConstraint]()
    queue ++= constraints

    while (queue.nonEmpty) {
      val constraint = queue.dequeue()
      val signature = matchSignature(constraint, signMap)
      require(signature.nonEmpty, s"Could not identify a signature for $constraint")
    }

    val p = new CSPOM()
    variables.foreach(p.addVariable)
    constraints.foreach(p.addConstraint)
    p
  }
}