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

  private def nextName(e: CSPOMExpression[_]) = e match {
    case CSPOMConstant(v)  => v.toString
    case CSPOMSeq(v) => v.map(names).mkString("CSPOMSeq(", ", ", ")")
    case _ =>
      id += 1
      "_" + id
  }

  def names(expression: CSPOMExpression[_]): String =
    generatedNames.getOrElseUpdate(expression, {
      val cspomNames = cspom.namesOf(expression)
      if (cspomNames.isEmpty) {
        nextName(expression)
      } else {
        cspomNames.mkString("||")
      }
    })

}
