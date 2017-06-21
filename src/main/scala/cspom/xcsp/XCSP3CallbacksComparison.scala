package cspom.xcsp

import cspom.CSPOMConstraint
import cspom.variable.{CSPOMConstant, CSPOMSeq}
import org.xcsp.common.Types.TypeOperatorRel
import org.xcsp.parser.entries.XVariables.XVarInteger

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksComparison extends XCSP3CallbacksVars {
  override def buildCtrAllDifferent(id: String, x: Array[XVarInteger]): Unit = {
    cspom.ctr('alldifferent)(toCspom(x): _*)
  }


  override def buildCtrAllDifferentList(id: String, lists: Array[Array[XVarInteger]]): Unit = {
    cspom.ctr('alldifferentList)(lists.toSeq.map(cspomSeq):_*)
    for (Array(a1, a2) <- lists.combinations(2)) {
      cspom.ctr('nevec)(cspomSeq(a1), cspomSeq(a2))
    }
  }

  override def buildCtrAllDifferentExcept(id: String, x: Array[XVarInteger], except: Array[Int]): Unit = {
    cspom.ctr(CSPOMConstraint('alldifferent)(toCspom(x): _*) withParam "except" -> except.toSeq)
  }

  override def buildCtrAllEqual(id: String, list: Array[XVarInteger]): Unit = {
    cspom.ctr('eq)(toCspom(list): _*)
  }

  override def buildCtrNotAllEqual(id: String, list: Array[XVarInteger]): Unit = {
    cspom.ctr(CSPOMConstraint(CSPOMConstant(false))('eq)(toCspom(list): _*))
  }

  override def buildCtrOrdered(id: String, list: Array[XVarInteger], operator: TypeOperatorRel): Unit = {
    cspom.ctr(CSPOMConstraint('ordered)(toCspom(list): _*) withParam "mode" -> operator.name)
  }


  override def buildCtrLex(id: String, list: Array[Array[XVarInteger]], operator: TypeOperatorRel): Unit = {
    cspom.ctr(CSPOMConstraint('lex)(list.toSeq.map(cspomSeq): _*) withParam "mode" -> operator.name)
  }


  override def buildCtrLexMatrix(id: String, list: Array[Array[XVarInteger]], operator: TypeOperatorRel): Unit = {
    cspom.ctr(CSPOMConstraint('lexmatrix)(CSPOMSeq(list.map(cspomSeq): _*)) withParam "mode" -> operator.name)
  }

  override def buildCtrAllDifferentMatrix(s: String, xVarIntegers: Array[Array[XVarInteger]]): Unit = {
    cspom.ctr('alldifferentMatrix)(CSPOMSeq(xVarIntegers.map(cspomSeq): _*))
  }

}
