package cspom.compiler

object StandardCompilers {

  def apply() = Seq(SplitEqVec, SplitAllEq, new ReduceRelations, MergeEq)

  def improve() = Seq(MergeSame, RemoveUselessEq)
}