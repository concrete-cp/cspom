package cspom

import cspom.variable.CSPOMVariable

/**
 * @author vion
 */
object GML {

  private def vars(problem: CSPOM): (Map[CSPOMVariable[_], String], Seq[String]) = {

    val vn = new VariableNames(problem)
    val v = problem.referencedExpressions
      .collect {
        case e: CSPOMVariable[_] => e -> vn.names(e)
      }
      .toMap

    val s = for (k <- v.values.toSeq.sorted) yield {
      s"""
          node [
            id "$k"
            label "$k"
          ]
          """
    }
    (v, s)
  }

  private def ctr(variables: Map[CSPOMVariable[_], String], constraints: Iterator[CSPOMConstraint[_]]) = {

    var gen = 0;

    constraints.flatMap { c =>
      c.fullScope.flatMap(_.flatten).collect {
        case v: CSPOMVariable[_] => variables(v)
      } match {
        case Seq(source, target) => s"""
          edge [
            source "$source"
            target "$target"
            label "${c.function.name}"
          ]
          """
        case s =>
          gen += 1
          s"""
          node [
            id "cons$gen"
            label "${c.function.name}"
            graphics [ fill "#FFAA00" ]
          ]
          """ ++ s.flatMap(v => s"""
          edge [
            source "cons$gen"
            target "$v"
          ]
          """)
      }
    }
  }

  /**
   * Generates the constraint network graph in the GML format. N-ary
   * constraints are represented as nodes.
   *
   * @return a String containing the GML representation of the constraint
   *         network.
   */
  def apply(problem: CSPOM): String = {

    val stb = new StringBuilder()
      .append("graph [\n")
      .append("directed 0\n")

    val (variables, strings) = vars(problem)

    strings.addString(stb)

    ctr(variables, problem.constraints).addString(stb)

    stb
      .append("]\n")
      .toString

  }
}