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
import cspom.variable.CSPOMExpression
import cspom.variable.IntExpression
import cspom.variable.BoolExpression
import cspom.util.RangeSet
import cspom.util.Infinitable
import cspom.util.Finite
import cspom.variable.FreeVariable
import cspom.util.IntInterval
import cspom.UNSATException

/**
 * Detects and removes constants from extensional constraints
 */
class ReduceRelations extends ConstraintCompilerNoData with LazyLogging {

  //private val cache = new HashMap[(IdEq[Relation[_]], Seq[SimpleExpression[_]]), (Seq[Int], Relation[Int])]

  override def matchBool(c: CSPOMConstraint[_], problem: CSPOM) = {
    //println(c)
    c.function == 'extension && c.nonReified
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM) = {

    val Some(relation: Relation[_]) = c.params.get("relation")

    //    val args: IndexedSeq[RangeSet[Infinitable]] = cargs.map {
    //      case IntExpression(e)  => IntExpression.implicits.ranges(e)
    //      case BoolExpression(e) => RangeSet(BoolExpression.span(e))
    //      case _: FreeVariable   => RangeSet.allInt
    //      case _                 => throw new IllegalArgumentException()
    //    }

    val args = c.arguments match {
      case SimpleExpression.simpleSeq(args) => args.toIndexedSeq
    }

    logger.info(s"will reduce $relation for $args")

    val filtered = relation.filter((k, i) => args(k).contains(i))

    logger.info(s"filtered: ${filtered ne relation}")

    if (filtered.isEmpty) logger.warn(s"Relation is empty for ${c.toString(problem.displayName)}")

    val vars = c.arguments.zipWithIndex.collect {
      case (c: CSPOMVariable[_], i) => i
    }

    if (vars.isEmpty) {
      logger.info("No variables left")
      removeCtr(c, problem)
    } else {

      val projected = if (vars.size < c.arguments.size) {
        filtered.project(vars)
      } else {
        filtered
      }

      logger.info(s"projected: ${projected ne filtered}")

      val reduced = projected match {
        case p: MDD[_] => p.reduce
        case p => p
      }

      logger.info(s"reduced: ${reduced ne projected}")

      if (relation ne reduced) {

        logger.info(s"$relation -> $reduced")
        replaceCtr(c,
          CSPOMConstraint('extension)(vars.map(args): _*) withParams (c.params + ("relation" -> reduced)),
          problem)

      } else {
        Delta.empty
      }

    }
  }

  def selfPropagation = false

}