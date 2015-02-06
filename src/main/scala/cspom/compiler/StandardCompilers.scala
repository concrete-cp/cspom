package cspom.compiler

object StandardCompilers {

  def apply() = Seq(SplitEqVec, SplitAllEq, ReduceRelations)

  def improve() = Seq(MergeSame, MergeEq, RemoveUselessEq)
}