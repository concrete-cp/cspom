package cspom.compiler

object StandardCompilers {

  def apply() = Seq(
    SplitEqVec,
    new ReduceRelations,
    MergeEq,
    MergeRelationsDepths)

  def improve() = Seq(MergeSame, RemoveUselessEq)
}