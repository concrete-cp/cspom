package cspom.compiler

import com.typesafe.scalalogging.LazyLogging
import cspom.{CSPOM, CSPOMConstraint}
import cspom.variable.CSPOMExpression

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


trait ACCSE[PairExp] extends ProblemCompiler with LazyLogging {

  def populate(c: CSPOMConstraint[_]): Iterator[PairExp]

  def replace(pair: PairExp, ls: Seq[CSPOMConstraint[_]], dn: CSPOMExpression[_] => String): Seq[CSPOMConstraint[_]]

  def apply(cspom: CSPOM): Delta = {
    val newConstraints = new mutable.HashSet[CSPOMConstraint[_]]
    val removed = new mutable.HashSet[CSPOMConstraint[_]]

    val map = new mutable.HashMap[PairExp, ArrayBuffer[CSPOMConstraint[_]]]
    populateMapAC(map, cspom.constraints.toSeq: _*)

    while (map.nonEmpty) {
      val (pairexp, ls0) = map.head
      map -= pairexp

      val ls = ls0.filter(c => newConstraints(c) || (cspom.constraintSet(c) && !removed(c)))

      if (ls.size > 1) {
        val added = replace(pairexp, ls, cspom.displayName)

        newConstraints ++= added
        for (c <- added) {
          populateMapAC(map, c)
        }

        // All constraints from ls are removed
        val (recentlyAdded, stillInProblem) = ls.partition(newConstraints)

        // Some are in new constraints
        newConstraints --= recentlyAdded

        logger.debug("Adding constraints")
        for (c <- added) {
          logger.debug(c.toString(cspom.displayName))
        }

        logger.debug("Removing recent constraints")
        for (c <- recentlyAdded) {
          logger.debug(c.toString(cspom.displayName))
        }

        logger.debug("Removing older constraints")
        for (c <- stillInProblem) {
          logger.debug(c.toString(cspom.displayName))
          assert(cspom.constraintSet(c))
        }

        // Some are in the original problem
        removed ++= stillInProblem
      }
    }

    ConstraintCompiler.replaceCtr(removed.toSeq, newConstraints.toSeq, cspom)
  }


  def populateMapAC(map: mutable.Map[PairExp, ArrayBuffer[CSPOMConstraint[_]]],
                    constraints: CSPOMConstraint[_]*): Unit = {
    for {
      c <- constraints
      pair <- populate(c)
    } {
      map.getOrElseUpdate(pair, new ArrayBuffer()) += c
    }
  }
}
