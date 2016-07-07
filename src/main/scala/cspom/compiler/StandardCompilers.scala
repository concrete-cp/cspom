package cspom.compiler

object StandardCompilers {

  def apply() = Seq(SplitEqVec, new ReduceRelations, MergeEq)

  def improve() = Seq(MergeSame, RemoveUselessEq)
}