package cspom.extension

import scala.collection.Seq
import language.experimental.macros

import scala.reflect.macros.blackbox.Context
import scala.util.Try

class Table[A](val table: Set[Seq[A]]) extends Relation[A] {

  def this(table: Seq[Seq[A]]) = this(table.toSet)

  // Members declared in scala.collection.IterableLike
  def iterator: Iterator[Seq[A]] = table.iterator
  // Members declared in cspom.extension.Relation
  def arity: Int = table.head.length
  def contains(t: Seq[A]): Boolean = table.exists(_ sameElements t)
  def filter(f: (Int, A) => Boolean) = {
    val filt = table.filter(t => t.zipWithIndex.forall { case (v, k) => f(k, v) })
    
    if (filt.size < table.size) {
      new Table(filt)
    } else {
      this
    }
    
  }

  override def toString = s"Table with $arity columns and $size rows"

  def project(c: Seq[Int]) = new Table(table.map(t => c.map(t)))

  def +(s: Seq[A]) = new Table(table + s)

  def -(s: Seq[A]) = new Table(table - s)

  override def equals(o: Any) = o match {
    case t: Table[A] => t.table == table
    case o           => super.equals(o)

  }

}

object Table {

  implicit class MatrixContext(sc: StringContext) {
    def matrix(): Array[Array[Int]] = macro matrixImpl
  }

  def matrixImpl(c: Context)(): c.Expr[Array[Array[Int]]] = {
    import c.universe.{ Try => _, _ }

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