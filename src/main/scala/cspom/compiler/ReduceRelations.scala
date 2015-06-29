package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.extension.Relation
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.HashMap
import cspom.extension.MDD
import scala.collection.mutable.WeakHashMap
import cspom.variable.IntVariable
import cspom.extension.IdEq
import cspom.variable.BoolVariable

/**
 * Detects and removes constants from extensional constraints
 */
object ReduceRelations extends ConstraintCompilerNoData with LazyLogging {

  val cache = new HashMap[(IdEq[Relation[_]], IndexedSeq[Set[Any]], Seq[Int]), Relation[_]]

  override def matchBool(c: CSPOMConstraint[_], problem: CSPOM) = {
    //println(c)
    c.function == 'extension && c.nonReified
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM) = {

    val Some(relation: Relation[Any] @unchecked) = c.params.get("relation")

    val args = c.arguments.toIndexedSeq

    val domains: IndexedSeq[Set[Any]] = args.map {
      case CSPOMConstant(v: Any) => Set(v)
      case v: IntVariable        => v.asSortedSet.asInstanceOf[Set[Any]]
      case b: BoolVariable       => Set[Any](false, true)
      case _                     => ???
    }

    val vars = c.arguments.zipWithIndex.collect {
      case (c: CSPOMVariable[_], i) => i
    }

    logger.info(s"will reduce $relation for $args")

    val cached = cache.getOrElseUpdate((IdEq(relation), domains, vars), {
      logger.info("reducing !")
      val filtered = relation.filter((k, i) => domains(k)(i))

      logger.trace(s"filtered: ${filtered ne relation}")

      val projected = if (vars.size < c.arguments.size) {
        filtered.project(vars)
      } else {
        filtered
      }

      logger.trace(s"projected: ${projected ne filtered}")

      val reduced = projected match {
        case p: MDD[_] => p.reduce
        case p         => p
      }

      logger.trace(s"reduced: ${reduced ne projected}")

      reduced
    })

    if (relation ne cached) {

      logger.info(s"$relation -> $cached")
      replaceCtr(c,
        CSPOMConstraint('extension)(vars.map(args): _*) withParams (c.withParam("relation" -> cached).params),
        problem)

    } else {
      Delta.empty
    }

  }

  def selfPropagation = false

}
