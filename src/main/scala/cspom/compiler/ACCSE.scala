package cspom.compiler

import java.util

import com.typesafe.scalalogging.LazyLogging
import cspom.{CSPOM, CSPOMConstraint}
import cspom.variable.{CSPOMExpression, CSPOMVariable, FreeVariable}

import scala.collection.mutable

trait ACCSE[PairExp, Data] extends ProblemCompiler with LazyLogging {

  type Arg = (CSPOMExpression[Any], Data)
  type Args = mutable.Map[CSPOMExpression[Any], Data]

  def filter(c: CSPOMConstraint[_]): Boolean

  def pair(a1: Arg, a2: Arg): PairExp

  def define(pair: PairExp, aux: CSPOMVariable[_]): (Arg, CSPOMConstraint[_])

  def replace(pair: PairExp, arg: Arg, constraint: Args): Boolean

  def constraintToArgs(c: CSPOMConstraint[_]): IndexedSeq[Arg]

  def argsToConstraint(original: CSPOMConstraint[_], args: Args): CSPOMConstraint[_]


  def toString(pair: PairExp, dn: CSPOMExpression[_] => String): String = pair.toString

  def toString(arg: Arg, dn: CSPOMExpression[_] => String): String = arg.toString

  def toHashMap[A, B](s: Iterable[(A, B)]): mutable.Map[A, B] = {
    val m = new mutable.HashMap[A, B]()
    m ++= s
    require(s.size == m.size)
    m
  }

  def apply(cspom: CSPOM): Delta = {
    val newConstraints = new mutable.HashSet[CSPOMConstraint[_]]

    val map = new mutable.HashMap[PairExp, List[Args]]().withDefaultValue(Nil)

    val constraints = new util.IdentityHashMap[Args, CSPOMConstraint[_]]
    val changed = new util.IdentityHashMap[Args, Unit]()

    for (c <- cspom.constraints if filter(c)) {
      val args = constraintToArgs(c)
      val mutableArgs = toHashMap(args)
      constraints.put(mutableArgs, c)

      for (i <- args.indices; j <- 0 until i) {
        val p = pair(args(i), args(j))
        map(p) ::= mutableArgs
      }
    }

    // Quickly filter singleton pairs
    map.retain((_, v) => v.lengthCompare(1) > 0) // values.removeIf(_.lengthCompare(1) <= 0)

    while (map.nonEmpty) {

      val (pairexp, list) = map.head

      // println(map.size)
      map -= pairexp

      if (list.size > 1) {

        val aux = new FreeVariable()
        val (commonArg, definition) = define(pairexp, aux)

        newConstraints += definition

        // println(s"New subexpression in ${list.size} constraints: ${aux.toString(cspom.displayName(_))} = ${toString(pairexp, cspom.displayName(_))}")
        // list.foreach(c => println(c.map(toString(_, cspom.displayName(_)))))

        val enqueue = new mutable.HashMap[PairExp, List[Args]]().withDefaultValue(Nil)

        for (c <- list) {
          if (replace(pairexp, commonArg, c)) {
            changed.put(c, ())
            //println(s"arity ${c.size}")
            for (a <- c if a._1 != aux) {
              val p = pair(a, commonArg)
              enqueue(p) ::= c
            }

          }
        }
        enqueue.retain((_, v) => v.lengthCompare(1) > 0) // values.removeIf(_.lengthCompare(1) <= 0)
        map ++= enqueue
      }
    }


    //println("End")

    var removed = Seq[CSPOMConstraint[_]]()
    var added = Seq[CSPOMConstraint[_]]()
    changed.keySet.forEach { c =>
      val constraint = constraints.get(c)
      removed +:= constraint
      added +:= argsToConstraint(constraint, c)
    }

//    removed.foreach(println)
//
//    println("Add")
//    added.foreach(println)

    ConstraintCompiler.replaceCtr(removed.toSeq, added.toSeq ++ newConstraints, cspom)

  }

}
