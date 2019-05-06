package cspom
package compiler

import com.typesafe.scalalogging.LazyLogging
import cspom.compiler.ConstraintCompiler._
import cspom.extension.{MDDRelation, Relation}
import cspom.variable.{CSPOMVariable, SimpleExpression}

/**
  * Detects and removes constants from extensional constraints
  */
class ReduceRelations extends ConstraintCompilerNoData with LazyLogging {

  def functions = Functions('extension)
  //private val cache = new HashMap[(IdEq[Relation[_]], Seq[SimpleExpression[_]]), (Seq[Int], Relation[Int])]

  override def matchBool(c: CSPOMConstraint[_], problem: CSPOM): Boolean = {
    //println(c)
    assert(c.function == 'extension)
    c.nonReified
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM): Delta = {

    val Some(relation: Relation[_]) = c.params.get("relation")

    //    val args: IndexedSeq[RangeSet[Infinitable]] = cargs.map {
    //      case IntExpression(e)  => IntExpression.implicits.ranges(e)
    //      case BoolExpression(e) => RangeSet(BoolExpression.span(e))
    //      case _: FreeVariable   => RangeSet.allInt
    //      case _                 => throw new IllegalArgumentException()
    //    }

    val args = c.arguments.map {
      case a: SimpleExpression[_] => a
    }
      .toIndexedSeq

    logger.info(s"will reduce $relation for $args")

    val filtered = relation.filter(args.map { expr: SimpleExpression[_] =>
      new Set[Int] {
        def contains(i: Int): Boolean = expr.contains(i)

        override def +(elem: Int): Set[Int] = ???

        override def -(elem: Int): Set[Int] = ???

        override def iterator: Iterator[Int] = ???
      }
    })

    logger.info(s"filtered: {}, {} -> {}", filtered != relation, relation.space, filtered.space)

    if (filtered.isEmpty) logger.warn(s"Relation is empty for ${c.toString(problem.displayName)}")

    val vars = c.arguments.zipWithIndex.collect {
      case (_: CSPOMVariable[_], i) => i
    }

    if (vars.isEmpty) {
      logger.info("No variables left")
      removeCtr(c, problem)
    } else {

      val projected = if (vars.lengthCompare(c.arguments.size) < 0) {
        filtered.project(vars)
      } else {
        filtered
      }

      logger.info(s"projected: ${projected != filtered}")

      val reduced = projected match {
        case p: MDDRelation => p.reduce()
        case p => p
      }

      logger.info(s"reduced: ${reduced != projected}")

      if (relation != reduced) {

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