package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.extension.Relation
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression
import com.typesafe.scalalogging.slf4j.LazyLogging

/**
 * Detects and removes constants from extensional constraints
 */
object ReduceRelations extends ConstraintCompilerNoData with LazyLogging {

  override def matchBool(c: CSPOMConstraint[_], problem: CSPOM) = c.function == 'extension && c.nonReified

  def compile(c: CSPOMConstraint[_], problem: CSPOM) = {

    val Some(relation: Relation[_]) = c.params.get("relation")

    val args = c.arguments.toIndexedSeq

    val domains = args.map {
      case v: SimpleExpression[Any] => v.domain.toSet
      case _ => ???
    }

    val filtered = relation.filter((k, i) => domains(k)(i))

    val vars = c.arguments.zipWithIndex.collect {
      case (c: CSPOMVariable[_], i) => i
    }
    val projected = if (vars.size < c.arguments.size) { filtered.project(vars) } else { filtered }

    logger.warn(relation + " -> " + projected)
    replaceCtr(c,
      CSPOMConstraint('extension, vars.map(args), c.params.updated("relation", projected)), problem)

  }

  def selfPropagation = false

}
