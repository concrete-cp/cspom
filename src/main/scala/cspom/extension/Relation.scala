package cspom.extension

import mdd.{MDD, MDD0, MiniSet}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.util.Try

trait Relation[A] extends Iterable[Seq[A]] {

  def tupleString = map {
    _.mkString(" ")
  } mkString "|"

  def contains(t: Seq[A]): Boolean

  def filter(f: IndexedSeq[MiniSet]): Relation[A]

  def project(c: Seq[Int]): Relation[A]

  def arity: Int

}

object MDDRelation {
  val empty = new MDDRelation(MDD0)

  def apply(t: Traversable[Seq[Int]]) = new MDDRelation(MDD(t))

}

class MDDRelation(val mdd: MDD) extends Relation[Int] {
  private lazy val modified = List.tabulate(arity)(identity)

  def iterator = mdd.iterator

  def contains(t: Seq[Int]) = mdd.contains(t.toArray)

  def arity = mdd.depth().getOrElse(-1)

  def filter(f: IndexedSeq[MiniSet]) = updated(mdd.filterTrie(f.toArray, modified))

  def project(c: Seq[Int]): MDDRelation = updated(mdd.project(c.toSet))

  def +(t: Seq[Int]) =  updated(mdd + t)

  def reduce() = updated(mdd.reduce())

  private def updated(mdd:MDD) = {
    if (mdd eq this.mdd) {
      this
    } else {
      new MDDRelation(mdd)
    }
  }

  override def toString = mdd.toString
}


object Relation {

  implicit class MatrixContext(sc: StringContext) {
    def matrix(): Array[List[Int]] = macro matrixImpl
  }

  def matrixImpl(c: Context)(): c.Expr[Array[List[Int]]] = {
    import c.universe.{Try => _, _}

    val matrix = Try {
      c.prefix.tree match {
        case Apply(_, List(Apply(_, List(Literal(Constant(raw: String)))))) =>

          def toArrayAST(c: List[TermTree]): Apply =
            Apply(Select(Select(Ident(TermName("scala")), TermName("Array")), TermName("apply")), c)

          val matrix = raw
            .split("\n")
            .map(_.trim)
            .filter(_.nonEmpty)
            .map {
              _.split(",").map(_.trim.toInt)
            }
          if (matrix.map(_.length).distinct.size != 1) {
            c.abort(c.enclosingPosition, "rows of matrix do not have the same length")
          }

          val matrixAST = matrix
            .map(_.map(i => Literal(Constant(i))))
            .map(i => toArrayAST(i.toList))

          toArrayAST(matrixAST.toList)
      }
    }

    c.Expr(matrix.getOrElse(c.abort(c.enclosingPosition, "not a matrix of Int")))
  }
}