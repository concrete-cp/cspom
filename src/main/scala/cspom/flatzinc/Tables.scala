package cspom.flatzinc

import cspom.extension.Table
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMConstant
import cspom.extension.MDD
import cspom.extension.MDDNode
import cspom.variable.CSPOMVariable
import cspom.extension.Relation
import cspom.variable.IntVariable
import cspom.variable.CSPOMExpression
import cspom.extension.MDDLeaf
import scala.collection.mutable.HashMap

object Tables {

  def element(as: CSPOMSeq[_]): Relation = {
    new Table(as.withIndex.map {
      case (CSPOMConstant(const: Int), i) => Seq(i, const)
    })
  }

  def elementVar(as: CSPOMSeq[_]): Relation = {
    val scope = as.values.asInstanceOf[Seq[IntVariable]].toArray

    val cache = new HashMap[(Int, Int, Int), MDD]()

    val map = for (i <- scope.indices) yield {
      as.definedIndices(i) -> new MDDNode(
        scope(i).domain.map {
          c => c -> elementVar(0, i, c, scope, cache)
        } toMap)
    }

    new MDDNode(map.toMap)

  }

  private def elementVar(current: Int, b: Int, c: Int, as: Array[IntVariable], cache: HashMap[(Int, Int, Int), MDD]): MDD = {
    if (current >= as.length) {
      MDDLeaf
    } else cache.getOrElseUpdate((current, b, c), {
      if (current == b) {
        new MDDNode(Map(c -> elementVar(current + 1, b, c, as, cache)))
      } else {
        new MDDNode(as(current).domain.map {
          v => v -> elementVar(current + 1, b, c, as, cache)
        } toMap)
      }
    })

  }
}