package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.extension.Relation
import cspom.variable.CSPOMConstant

/**
 * Detects and removes constants from extensional constraints
 */
object ReduceRelations extends ConstraintCompiler {

  type A = Map[Int, Int]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM): Option[A] = {
    if (c.function == 'extension && c.result == CSPOMTrue) {
      val constants = c.arguments.zipWithIndex.collect {
        case (v: CSPOMConstant[Int], i) => (i, v.value)
      }
      if (constants.nonEmpty) {
        Some(constants.toMap)
      } else {
        None
      }

    } else {
      None
    }

  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val Some(relation: Relation) = c.params.get("relation").collect { case r: Relation => r }

    val filtered = relation.filter((k, i) => data.get(k).forall(_ == i))

    val (scope, pos) = c.arguments.zipWithIndex.filterNot { case (_, i) => data.contains(i) }.unzip

    val projected = filtered.project(pos)

    replaceCtr(c, CSPOMConstraint('extension, scope, c.params.updated("relation", projected)), problem)

  }

  def selfPropagation = false

}