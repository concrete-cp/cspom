package cspom.extension
import language.experimental.macros

import scala.reflect.macros.blackbox.Context
import scala.util.Try

trait Relation[A] extends Iterable[Seq[A]] {

  def tupleString = map { _.mkString(" ") } mkString "|"

  def contains(t: Seq[A]): Boolean

  def filter(f: (Int, A) => Boolean): Relation[A]

  def project(c: Seq[Int]): Relation[A]

  def arity: Int

}

object Relation {

  implicit class MatrixContext(sc: StringContext) {
    def matrix(): Array[List[Int]] = macro matrixImpl
  }

  def matrixImpl(c: Context)(): c.Expr[Array[List[Int]]] = {
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