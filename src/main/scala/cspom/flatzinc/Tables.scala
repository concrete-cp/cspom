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

  def element[A >: Int](as: CSPOMSeq[A]): Relation[A] = {
    new Table(as.withIndex.map {
      case (CSPOMConstant(const: A), i) => Seq(i, const)
    })
  }

  def elementVar[A >: Int](as: CSPOMSeq[A]): Relation[A] = {
    val scope = as.values.asInstanceOf[Seq[CSPOMVariable[A]]].toArray

    val cache = new HashMap[(Int, Int, A), MDD[A]]()

    val map: Seq[(A, MDD[A])] = for (i <- scope.indices) yield {
      as.definedIndices(i) -> new MDDNode(
        scope(i).domain.map {
          c => c -> elementVar(0, i, c, scope, cache)
        } toMap)
    }

    new MDDNode(map.toMap)

  }

  private def elementVar[A >: Int](current: Int, b: Int, c: A, as: Array[CSPOMVariable[A]], cache: HashMap[(Int, Int, A), MDD[A]]): MDD[A] = {
    if (current >= as.length) {
      MDD.leaf
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