package cspom.compiler

import java.util

import com.typesafe.scalalogging.LazyLogging
import cspom.{CSPOM, CSPOMConstraint}
import cspom.variable.{CSPOMExpression, CSPOMVariable, FreeVariable}

import scala.collection.mutable

trait ACCSE[Data] extends ProblemCompiler with LazyLogging {

  protected type Arg = (CSPOMExpression[Any], Data)
  protected type Args = mutable.Map[CSPOMExpression[Any], Data]

  def functions: Seq[String]

  /**
    * Converts a list of Args to SubExp
    *
    * @param args
    * @return
    */
  def canonize(args: List[Arg]): List[Arg]

  private def subExp(args: List[Arg]) = {
    // Variables are always given in the same order -- avoids using Sets
    val sorted = args.sortBy { case (x, _) => x.hashCode }
    canonize(sorted)
  }

  /**
    * Finds an intersection between two subexps
    * @param se1
    * @param se2
    * @return
    */
  def intersect(se1: List[Arg], se2: List[Arg], including: List[Arg]): List[Arg]

  def define(pair: List[Arg], aux: CSPOMVariable[_]): (Arg, CSPOMConstraint[_])

  def replace(pair: List[Arg], arg: Arg, constraint: Args): Option[Arg]

  def constraintToArgs(c: CSPOMConstraint[_]): IndexedSeq[Arg]

  def argsToConstraint(original: CSPOMConstraint[_], args: Args): CSPOMConstraint[_]

  /**
    * Checks whether pair is still contained in given args
    *
    * @param pair
    * @param args
    * @return
    */
  private def isValid(pair: List[Arg], args: Args): Boolean = {
    pair.forall { case (x, _) => args.contains(x) }
  }

  def toString(pair: List[Arg], dn: CSPOMExpression[_] => String): String = pair.toString

  def toString(arg: Arg, dn: CSPOMExpression[_] => String): String = arg.toString

  def toHashMap[A, B](s: Iterable[(A, B)]): mutable.Map[A, B] = {
    val m = new mutable.HashMap[A, B]()
    m ++= s
    assert(s.size == m.size, s"$s contains duplicates")
    m
  }


  def apply(cspom: CSPOM): Delta = {
    val newConstraints = new mutable.HashSet[CSPOMConstraint[_]]

    val map = new mutable.HashMap[List[Arg], List[Args]]().withDefaultValue(Nil)

    val constraints = new util.IdentityHashMap[Args, CSPOMConstraint[_]]
    val changed = new util.IdentityHashMap[Args, Unit]()

    for (f <- functions; c <- cspom.getConstraints(f)) {
      val args = constraintToArgs(c)
      val mutableArgs = toHashMap(args)
      constraints.put(mutableArgs, c)

      for (i <- args.indices; firstArg = List(args(i)); j <- 0 until i) {
        val p = subExp(args(j) :: firstArg)
        map(p) ::= mutableArgs
      }
    }

    // Quickly filter singleton pairs
    map.filterInPlace((_, v) => v.lengthCompare(1) > 0) // values.removeIf(_.lengthCompare(1) <= 0)

    while (map.nonEmpty) {

      for (pairexp <- map.toSeq.sortBy(-_._2.size).map(_._1)) {
        // println(map.size)
        val list: List[Args] = map.remove(pairexp).get
          .filter(args => isValid(pairexp, args))

        if (list.lengthCompare(1) > 0) {
          val cs = list.map { a: Args => subExp(a.toList) }.reduce((se1, se2) => intersect(se1, se2, pairexp))

          // println(pairexp, list, cs)

          //  println(cs)

          val aux = new FreeVariable()
          val (commonArg, definition) = define(cs, aux) //define(pairexp, aux)

          newConstraints += definition

          // println(s"New SE: ${aux.toString(cspom.displayName)} = ${definition.toString(cspom.displayName)} for ${list.length} constraints")
          // list.foreach(c => println(c.map { case (k, v) => s"${k.toString(cspom.displayName)} -> $v" }))

          // println(s"New subexpression in ${list.size} constraints: ${aux.toString(cspom.displayName(_))} = ${toString(pairexp, cspom.displayName(_))}")
          // list.foreach(c => println(c.map(toString(_, cspom.displayName(_)))))

          val enqueue = new mutable.HashMap[List[Arg], List[Args]]().withDefaultValue(Nil)

          for (c <- list; newArg <- replace(cs, commonArg, c)) {
            changed.put(c, ())
            //println(s"arity ${c.size}")
            for (a <- c if a._1 != aux) {
              val p = subExp(List(a, newArg))
              enqueue(p) ::= c
            }
          }
          for (entry <- enqueue if entry._2.lengthCompare(1) > 0) {
            map += entry
          }
        }
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

//    println(s"ACSSE will remove ${removed.size} constraints")
//    println(s"ACSSE defines ${newConstraints.size} constraints")
//    println(s"ACSSE will add ${added.size} constraints")

    //    removed.foreach(println)
    //
    //    println("Add")
    //    added.foreach(println)

    ConstraintCompiler.replaceCtr(removed, added ++ newConstraints, cspom)

  }

}
