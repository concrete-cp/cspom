package cspom.compiler

object StandardCompilers {
  def apply() = Seq(MergeSame, MergeEq)
}