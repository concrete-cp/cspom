package cspom.compiler

import com.typesafe.scalalogging.LazyLogging
import cspom.extension.MDDRelation
import cspom.variable.CSPOMExpression
import cspom.{CSPOM, CSPOMConstraint}

import scala.collection.mutable.ArrayBuffer

/**
  * Detects and removes constants from extensional constraints
  */
object MergeRelationsDepths extends ConstraintCompilerNoData with LazyLogging {

  def functions = Functions('extension)
  //private val cache = new HashMap[(IdEq[Relation[_]], Seq[SimpleExpression[_]]), (Seq[Int], Relation[Int])]

  override def matchBool(c: CSPOMConstraint[_], problem: CSPOM): Boolean = {
    //println(c)
    assert(c.function == 'extension)
    c.arguments.distinct != c.arguments
  }


  def compile(c: CSPOMConstraint[_], problem: CSPOM): Delta = {


    var Some(relation: MDDRelation) = c.params.get("relation")

    var args = c.arguments

    val newArgs = new ArrayBuffer[CSPOMExpression[_]]()

    logger.info(s"will merge $relation for $args")

    while (args.nonEmpty) {
      val arg = args.head
      val indices = args.indices.filter(i => args(i) == arg)
      if (indices.size > 1) {
        logger.debug("Merging {} in {}", indices, args.map(problem.namesOf))
        relation = relation.merge(indices.toList.map(_ + newArgs.size))
      }
      args = args.zipWithIndex.filterNot(e => indices.contains(e._2))
        .map(_._1)

      newArgs += arg
    }

    logger.info(s"obtained $relation for $newArgs")

    ConstraintCompiler.replaceCtr(c,
      CSPOMConstraint('extension)(newArgs: _*) withParams (c.params + ("relation" -> relation)),
      problem)


  }


}