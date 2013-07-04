package cspom.flatzinc

import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMDomain
import cspom.variable.AuxVar
import cspom.variable.ProblemVar

sealed trait FZVariableType {
  def genVariables(varParId: String, ann: Seq[String]): FZVariableDecl

}

case class FZVariableArray(val indices: Range, val d: CSPOMDomain[Any]) extends FZVariableType {
  def genVariables(varParId: String, ann: Seq[String]) = {
    val offset = indices.head
    val size = indices.size

    val variables: IndexedSeq[CSPOMVariable] =
      if (ann.contains("var_is_introduced")) {
        indices.map(i => new AuxVar(s"$varParId[$i]", Some(d)))
      } else {
        indices.map(i => new ProblemVar(s"$varParId[$i]", d))
      }

    FZVarDeclArray(varParId, offset, variables)
  }
}

case class FZSingleVariable(val d: CSPOMDomain[Any]) extends FZVariableType {
  def genVariables(varParId: String, ann: Seq[String]) = FZSingleVarDecl(
    if (ann.contains("var_is_introduced")) {
      new AuxVar(varParId, Some(d))
    } else {
      new ProblemVar(varParId, d)
    })

}

sealed trait FZVariableDecl {
  def getVars: Seq[CSPOMVariable]
  def name: String
}

case class FZVarDeclArray(val name: String, val offset: Int, val array: IndexedSeq[CSPOMVariable]) extends FZVariableDecl {
  override def toString = s"array[$offset..${offset + array.size}]: ${array.mkString(", ")}"

  def getVars = array
}

case class FZSingleVarDecl(val variable: CSPOMVariable) extends FZVariableDecl {
  def getVars = Seq(variable)
  def name = variable.name
}