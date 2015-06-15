package cspom.compiler

object StandardCompilers {

  def apply() = Seq(SplitEqVec, SplitAllEq, ReduceRelations, MergeEq)

  def improve() = Seq(MergeSame, RemoveUselessEq)
}