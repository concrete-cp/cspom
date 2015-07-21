package cspom

import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import scala.collection.mutable

final class VariableNames(cspom: CSPOM) {

  val generatedNames = mutable.Map[CSPOMExpression[_], String]()
  //
  //  for ((n, v) <- cspom.namedExpressions) {
  //    name(v, n)
  //  }
  //
  //  private def add(e: CSPOMExpression[_], n: String) {
  //    generatedNames(e) = generatedNames.get(e) match {
  //      case Some(ns) => s"$ns||$n"
  //      case None => n
  //    }
  //  }
  //
  //  private def name(e: CSPOMExpression[_], root: String): Unit = {
  //    add(e, root)
  //
  //    e match {
  //      case CSPOMSeq(v, i, _) => for ((v, i) <- (v zip i)) {
  //        name(v, s"$root[$i]")
  //      }
  //      case _ =>
  //    }
  //  }

  var id = 0

  val nextName: CSPOMExpression[_] => String = {
    case CSPOMConstant(v) => v.toString
    case CSPOMSeq(v)      => v.map(names).mkString("CSPOMSeq(", ", ", ")")
    case _ =>
      id += 1
      "_" + id
  }

  val names: CSPOMExpression[_] => String = {
    case CSPOMConstant(c) => c.toString
    case expression =>
      cspom.namesOf(expression).toSeq match {
        case Seq()      => generatedNames.getOrElseUpdate(expression, nextName(expression))
        case cspomNames => cspomNames.sorted.mkString("||")
      }
  }

}
