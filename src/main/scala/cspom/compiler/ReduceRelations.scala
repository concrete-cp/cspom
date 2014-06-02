package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.extension.Relation
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection.mutable.HashMap
import cspom.extension.MDD
import scala.collection.mutable.WeakHashMap
import cspom.variable.IntVariable

/**
 * Detects and removes constants from extensional constraints
 */
object ReduceRelations extends ConstraintCompilerNoData with LazyLogging {

  val cache = new WeakHashMap[Relation[_], Relation[_]]

  override def matchBool(c: CSPOMConstraint[_], problem: CSPOM) = c.function == 'extension && c.nonReified

  def compile(c: CSPOMConstraint[_], problem: CSPOM) = {

    val Some(r: Relation[_]) = c.params.get("relation")

    val relation = r.asInstanceOf[Relation[Any]]
    
    val args = c.arguments.toIndexedSeq

    val domains: IndexedSeq[Set[Any]] = args.map {
      case CSPOMConstant(v: Any) => Set(v)
      case v: IntVariable => v.asSortedSet.asInstanceOf[Set[Any]]
      case _ => ???
    }

    val filtered = relation.filter((k, i) => domains(k)(i))

    val vars = c.arguments.zipWithIndex.collect {
      case (c: CSPOMVariable[_], i) => i
    }
    val projected = if (vars.size < c.arguments.size) { filtered.project(vars) } else { filtered }

    val cached = cache.getOrElseUpdate(projected, projected)

    logger.info(s"$relation -> $cached")
    replaceCtr(c,
      CSPOMConstraint('extension, vars.map(args), c.params.updated("relation", cached)),
      problem)

  }

  def selfPropagation = false

}
