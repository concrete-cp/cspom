package cspom.xcsp

import cspom.extension.MDDRelation
import cspom.variable.CSPOMConstant
import cspom.{CSPOM, CSPOMConstraint}
import mdd.{JavaMap, MDD, MDDLeaf}
import org.xcsp.parser.entries.XVariables.XVarInteger

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksLanguage extends XCSP3CallbacksVars {

  override def buildCtrRegular(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]], startState: String, finalStates: Array[String]): Unit = {
    val states = transitions.flatMap { case Array(i: String, _, o: String) => Seq(i, o) }.distinct.zipWithIndex.toMap
    val dfa = transitions
      .view
      .map {
        case Array(i: String, v: Any, o: String) =>
          (states(i), v.asInstanceOf[Any]) -> states(o)
      }
      .toMap

    val intFinal: Seq[Int] = finalStates.map(states)

    cspom.ctr(
      CSPOMConstraint('regular)(
        cspomSeq(list),
        CSPOMConstant(states(startState)),
        CSPOM.constantSeq(intFinal)) withParam ("dfa" -> dfa))
  }

  override def buildCtrMDD(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]]): Unit = {
    val nodeNames = transitions.groupBy(x => x(0))

    val nodes = new JavaMap[String, MDD]()
    nodes.put("nodeT", MDDLeaf)

    def buildMDD(node: String): MDD = {
      nodes.getOrElseUpdate(node, {
        val newVal =
          MDD.fromTrie(
            nodeNames(node).view
              .map {
                case Array(_, value: java.lang.Long, target: String) =>
                  java.lang.Math.toIntExact(value) -> buildMDD(target)
              }
          )
        // nodes.put(node, newVal)
        newVal
      })
    }

    cspom.ctr(CSPOM.IntSeqOperations(toCspom(list)) in new MDDRelation(buildMDD("root")))

  }
}
