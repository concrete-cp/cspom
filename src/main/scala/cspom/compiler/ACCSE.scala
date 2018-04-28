package cspom.compiler

import java.util

import com.typesafe.scalalogging.LazyLogging
import cspom.{CSPOM, CSPOMConstraint}
import cspom.variable.CSPOMExpression

import scala.collection.mutable

trait ACCSE[PairExp] extends ProblemCompiler with LazyLogging {

  def populate(c: CSPOMConstraint[_]): Iterator[PairExp]

  def replace(pair: PairExp, ls: Seq[CSPOMConstraint[_]], dn: CSPOMExpression[_] => String): Seq[CSPOMConstraint[_]]

  def apply(cspom: CSPOM): Delta = {
    val newConstraints = new mutable.HashSet[CSPOMConstraint[_]]
    val removed = new mutable.HashSet[CSPOMConstraint[_]]

    val map = populateMapAC(
      new util.HashMap[PairExp, List[CSPOMConstraint[_]]](), cspom.constraints.toSeq: _*)

    map.values.removeIf(_.lengthCompare(1) <= 0)

    while (!map.isEmpty) {
      val entry = map.entrySet.iterator.next()
      val pairexp = entry.getKey

      map.remove(pairexp)

      val ls = entry.getValue.filter(c => newConstraints(c) || (cspom.constraintSet(c) && !removed(c)))

      if (ls.lengthCompare(1) > 0) {
        val added = replace(pairexp, ls, cspom.displayName)

        newConstraints ++= added
        for (c <- added) {
          populateMapAC(map, c)
        }

        // All constraints from ls are removed
        val (recentlyAdded, stillInProblem) = ls.partition(newConstraints)

        // Some are in new constraints
        newConstraints --= recentlyAdded

        //        logger.debug("Adding constraints")
        //        for (c <- added) {
        //          logger.debug(c.toString(cspom.displayName))
        //        }
        //
        //        logger.debug("Removing recent constraints")
        //        for (c <- recentlyAdded) {
        //          logger.debug(c.toString(cspom.displayName))
        //        }
        //
        //        logger.debug("Removing older constraints")
        //        for (c <- stillInProblem) {
        //          logger.debug(c.toString(cspom.displayName))
        //          assert(cspom.constraintSet(c))
        //        }

        // Some are in the original problem
        removed ++= stillInProblem
      }
    }

    ConstraintCompiler.replaceCtr(removed.toSeq, newConstraints.toSeq, cspom)
  }


  def populateMapAC(map: util.HashMap[PairExp, List[CSPOMConstraint[_]]],
                    constraints: CSPOMConstraint[_]*): util.HashMap[PairExp, List[CSPOMConstraint[_]]] = {
    for {
      c <- constraints
      pair <- populate(c)
    } {
      map.put(pair, c :: map.getOrDefault(pair, Nil))
    }
    map
  }
}
