package cspom.variable


trait CSPOMExpression {
  def variables: Seq[CSPOMVariable]
}

final case class CSPOMSeq(seq: Seq[CSPOMExpression]) {
  def variables = seq
} 
