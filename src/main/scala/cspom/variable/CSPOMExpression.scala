package cspom.variable

/*
 * An expression can be either a variable, a constant or a variable sequence
 */
trait CSPOMExpression {
  //def variables: Seq[CSPOMVariable]
}

final case class CSPOMSeq(seq: Seq[CSPOMExpression]) {
  //def variables = seq
} 

final case class CSPOMConstant(value: Any) {
  //def 
}