package cspom.compiler

object StandardCompilers {
  def apply() = Seq(SplitEqVec, SplitAllEq, MergeSame, MergeEq, RemoveUselessEq)
}