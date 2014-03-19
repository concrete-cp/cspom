package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.extension.Relation
import cspom.variable.CSPOMConstant

/**
 * Detects and removes constants from extensional constraints
 */
object ReduceRelations extends ConstraintCompiler {

  type A = Map[Int, Any]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM): Option[A] = c match {
    case CSPOMConstraint(CSPOMConstant(true), 'extension, args, _) =>
      val constants = args.zipWithIndex.collect {
        case (CSPOMConstant(value: Any), i) => i -> value
      }
      if (constants.nonEmpty) {
        Some(constants.toMap)
      } else {
        None
      }

    case _ =>
      None

  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val Some(relation: Relation[_]) = c.params.get("relation")

    val filtered = relation.filter((k, i) => data.get(k).forall(_ == i))

    val (scope, pos) = c.arguments.zipWithIndex.filterNot {
      case (_, i) => data.contains(i)
    }.unzip

    val projected = filtered.project(pos)
    println(relation + " -> " + projected)
    replaceCtr(c,
      CSPOMConstraint('extension, scope, c.params.updated("relation", projected)), problem)

  }

  def selfPropagation = false

}
