package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.extension.Relation
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression
import cspom.Loggable

/**
 * Detects and removes constants from extensional constraints
 */
object ReduceRelations extends ConstraintCompiler with Loggable {

  type A = Seq[Int]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM): Option[A] = c match {
    case CSPOMConstraint(CSPOMConstant(true), 'extension, args, _) =>
      val variables = args.zipWithIndex.collect {
        case (c: CSPOMVariable[_], i) => i
      }
      if (variables.size < args.size) {
        Some(variables)
      } else {
        None
      }

    case _ =>
      None

  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, vars: A) = {
    val Some(relation: Relation[_]) = c.params.get("relation")

    val args = c.arguments.toIndexedSeq
    
    val domains = args.map {
      case v: SimpleExpression[Any] => v.domain.toSet
      case _ => ???
    }

    val filtered = relation.filter((k, i) => domains(k)(i))

    val projected = filtered.project(vars)
    logger.info(relation + " -> " + projected)
    replaceCtr(c,
      CSPOMConstraint('extension, vars.map(args), c.params.updated("relation", projected)), problem)

  }

  def selfPropagation = false

}
