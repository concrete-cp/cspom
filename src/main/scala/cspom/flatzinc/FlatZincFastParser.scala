package cspom
package flatzinc

import java.io.InputStream

import cspom.flatzinc.MZWhitespace._
import cspom.variable.CSPOMExpression
import fastparse._

import scala.annotation.{switch, tailrec}
import scala.io.Source
import scala.util.Try

/**
  * Whitespace syntax that supports % line-comments for MiniZinc
  */
object MZWhitespace {
  implicit val whitespace: P[_] => P[Unit] = { implicit ctx: ParsingRun[_] =>
    val input = ctx.input

    @tailrec
    def rec(current: Int, state: Int): ParsingRun[Unit] = {
      if (!input.isReachable(current)) ctx.freshSuccessUnit(current)
      else {
        val currentChar = input(current)
        (state: @switch) match {
          case 0 =>
            (currentChar: @switch) match {
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
              case '%' => rec(current + 1, state = 1)
              case _ => ctx.freshSuccessUnit(current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state)
        }
      }
    }

    rec(current = ctx.index, state = 0)
  }
}

object FlatZincFastParser extends CSPOM.Parser {

  def fzAnnotations(ann: Seq[FZAnnotation]): Seq[(String, Seq[FZAnnotation])] = {
    if (ann.isEmpty) {
      Seq()
    } else {
      Seq("fzAnnotations" -> ann)
    }
  }

  def apply(is: InputStream): Try[CSPOM] = {
    Try {
      parse(Source.fromInputStream(is).mkString, flatzincModel(_))
    }
      .flatMap {
        case Parsed.Success(cspom, _) => scala.util.Success(cspom)
        case f: Parsed.Failure =>
          scala.util.Failure(new CSPParseException(f.trace().longAggregateMsg, null, f.index))
      }
  }

  /*
   * Definition of what's a flatzinc file : predicate(s) + parameter(s) + constraint(s) + solve goal
   */
  def flatzincModel[_: P]: P[CSPOM] = P(Start ~ pred_decl.rep ~/ (param_decl | var_decl).rep ~/ constraint.rep ~/ solve_goal ~/ End).map {
    case (predicates, paramOrVar, constraints, goal) =>

      val params: Map[String, CSPOMExpression[Any]] = paramOrVar.collect {
        case FZParamDecl(name, expr) => name -> expr
      }.toMap

      val variables = paramOrVar.collect {
        case v: FZVarDecl[_] => v
      }

      assert(params.size + variables.size == paramOrVar.size)

      val declared = variables.foldLeft(params) {
        case (vars, FZVarDecl(name, expression, _)) => vars + (name -> expression.fold(_.genVariable(), _.toCSPOM(vars)))
      }

      val annotations: Map[String, Seq[FZAnnotation]] = variables.map {
        case FZVarDecl(name, _, ann) => name -> ann
      }
        .toMap

      CSPOM { implicit problem =>

        for ((name, expr) <- declared) {
          expr as name
          for (
            annotation <- annotations.get(name);
            (title, ann) <- fzAnnotations(annotation)
          ) {
            problem.addAnnotation(name, title, ann)
          }

        }

        for (FZConstraint(predAnnId, expr, annotations) <- constraints) {
          CSPOM.ctr(
            CSPOMConstraint(Symbol(predAnnId))(expr.map(_.toCSPOM(declared)): _*) withParam
              (fzAnnotations(annotations): _*))
        }

        CSPOM.goal {
          val FZSolve(mode, ann) = goal
          val params = Map("fzSolve" -> ann)
          mode match {
            case Satisfy => WithParam(CSPOMGoal.Satisfy, params)
            case Minimize(e: FZExpr[Int]) =>
              val ce = e.toCSPOM(declared) //.asInstanceOf[CSPOMExpression[Int]]
              WithParam(CSPOMGoal.Minimize(ce), params)
            case Maximize(e: FZExpr[Int]) =>
              val ce = e.toCSPOM(declared) //.asInstanceOf[CSPOMExpression[Int]]
              WithParam(CSPOMGoal.Maximize(ce), params)
          }
        }
      }
  }

  def pred_ann_id[_: P]: P[String] = P(CharIn("a-zA-Z") ~~ CharIn("_0-9a-zA-Z").repX).!

  //
  def var_par_id[_: P]: P[FZVarParId[Any]] =
    P("_".repX ~~ CharIn("a-zA-Z") ~~ CharIn("_0-9a-zA-Z").repX).!.map { id =>
      FZVarParId[Any](id)
    }

  def bool_const[_: P]: P[FZBoolConst] = P("true").map(_ => FZBoolConst(true)) |
    P("false").map(_ => FZBoolConst(false))

  def float_const[_: P]: P[FZFloatConst] = P(
    CharIn("+\\-").? ~~ CharIn("0-9").repX(1) ~~ "." ~~ CharIn("0-9").repX(1) ~~ (
      CharIn("eE") ~~ CharIn("+\\-").? ~~ CharIn("0-9").repX(1)).?).!.map {
    f => FZFloatConst(f.toDouble)
  }

  def int_const[_: P]: P[FZIntConst] = P(
    CharIn("+\\-").? ~~ CharIn("0-9").repX(1)
  ).!.map { i => FZIntConst(i.toInt) }

  def set_const[_: P]: P[FZSetConst] = {
    P(int_const ~ ".." ~/ int_const).map { case (i, j) => FZSetConst(i.value to j.value) } |
      P("{" ~ int_const.rep(sep = ",") ~ "}").map(i => FZSetConst(i.map(_.value)))
  }

  def pred_decl[_: P]: P[Any] = P("predicate" ~ pred_ann_id ~ "(" ~/ pred_param.rep(sep = ",") ~ ")" ~ ";").map {
    case (a, p) => FZPredicate(a, p)
  }

  def index_set1[_: P]: P[Option[Range]] =
    (P("1..") ~ int_const).map { i => Some(1 to i.value) } |
      P("int").map(_ => None)

  def index_set[_: P]: P[Seq[Option[Range]]] = index_set1.rep(sep = ",")

  def single_var_type[_: P]: P[FZVarType[_]] =
    P("var" ~ "bool").map(_ => FZBoolean) |
      P("var" ~ "float").map(_ => FZFloat) |
      P("var" ~ float_const ~ ".." ~ float_const).map {
        case (lb, ub) => FZFloatInterval(lb.value, ub.value)
      } |
      P("var" ~ "int").map(_ => FZInt) |
      P("var" ~ int_const ~ ".." ~ int_const).map {
        case (lb, ub) => FZIntInterval(lb.value, ub.value)
      } |
      P("var" ~ "{" ~ int_const.rep(sep = ",") ~ "}").map { list =>
        FZIntSeq(list.map(_.value))
      } |
      P("var" ~ "set" ~ "of" ~ int_const ~ ".." ~ int_const).map(_ => throw new AssertionError("Unsupported domain type")) |
      P("var" ~ "set" ~ "of" ~ "{" ~ int_const.rep(sep = ",") ~ "}").map(_ => throw new AssertionError("Unsupported domain type"))

  def var_type[_: P]: P[FZVarType[Any]] = {
    single_var_type |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ single_var_type).map {
        case (indices, vartype) => FZArray(indices, vartype)
      }
  }

  def var_pred_param_type[_: P]: P[Any] =
    var_type |
      "var set of int" |
      "array" ~ "[" ~ index_set ~ "] of var set of int"

  //

  def par_type[_: P]: P[FZVarType[AnyVal]] =
    P("bool").map(_ => FZBoolean) |
      P("float").map(_ => FZFloat) |
      P("int").map(_ => FZInt) |
      P("set of int").map(_ => FZIntSet) |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "bool").map {
        FZArray[Boolean](_, FZBoolean)
      } |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "float").map { s => FZArray[Double](s, FZFloat) } |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "int").map {
        FZArray[Int](_, FZInt)
      } |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "set" ~ "of" ~ "int").map {
        FZArray[Int](_, FZIntSet)
      }

  def par_pred_param_type[_: P]: P[Any] =
    par_type |
      float_const ~ ".." ~/ float_const |
      int_const ~ ".." ~/ int_const |
      P("{" ~/ int_const.rep(sep = ",") ~ "}") |
      P("set" ~ "of" ~ int_const ~ ".." ~/ int_const) |
      P("set" ~ "of" ~ "{" ~/ int_const.rep(sep = ",") ~ "}") |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ float_const ~ ".." ~/ float_const) |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ int_const ~ ".." ~/ int_const) |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "{" ~/ int_const.rep(sep = ",") ~ "}") |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "set" ~ "of" ~ int_const ~ ".." ~/ int_const) |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "set" ~ "of" ~ int_const ~ ".." ~/ int_const)

  def pred_param_type[_: P]: P[Any] = par_pred_param_type | var_pred_param_type

  def pred_param[_: P]: P[Any] = pred_param_type ~ ":" ~ pred_ann_id

  //

  def StringChars(c: Char): Boolean = !"\"\\".contains(c)

  def escape[_: P]: P[Unit] = P("\\" ~ CharIn("\"/\\bfnrt"))

  def strChars[_: P]: P[Unit] = P(CharsWhile(StringChars))

  def stringLiteral[_: P]: P[String] =
    P("\"" ~/ (strChars | escape).rep.! ~ "\"")

  def array_expr[_: P]: P[FZArrayExpr[Any]] = {
    P("[" ~ expr.rep(sep = ",") ~ "]").map { l => FZArrayExpr(l) }
  }

  def array_exprInAnn[_: P]: P[FZArrayExpr[Any]] = {
    P("[" ~ exprInAnn.rep(sep = ",") ~ "]").map { l => FZArrayExpr(l) }
  }

  def expr[_: P]: P[FZExpr[Any]] = {
    bool_const |
      set_const |
      float_const |
      int_const |
      (var_par_id ~ "[" ~/ int_const ~ "]").map {
        case (varParId, index) => varParId.index(index.value)
      } |
      var_par_id |
      array_expr
  }

  //
  def annotation[_: P]: P[FZAnnotation] = {
    P(pred_ann_id ~ ("(" ~ exprInAnn.rep(sep = ",") ~ ")").?).map {
      case (id, expressions) => FZAnnotation(id, expressions.toSeq.flatten)
    }
  }

  def annotations[_: P]: P[Seq[FZAnnotation]] = P("::" ~ annotation).rep

  //
  def exprInAnn[_: P]: P[FZExpr[Any]] = P(
    array_exprInAnn |
      annotation |
      expr |
      stringLiteral.map(FZStringLiteral))

  def param_decl[_: P]: P[FZParamDecl[Any]] = P(par_type ~ ":" ~/ var_par_id ~ "=" ~/ expr ~ ";"./).map {
    case (t, id, expr) =>
      val e: CSPOMExpression[_] = (t, expr) match {
        case (FZArray(Seq(indices), _), a: FZArrayExpr[_]) => a.asConstant(indices.get)
        case (_, c: FZConstant[_]) => c.asConstant
        case _ => throw new IllegalArgumentException("Constant expected")
      }

      FZParamDecl(id.ident, e)
  }

  def var_decl[_: P]: P[FZVarDecl[Any]] = {
    P(var_type ~ ":" ~/ var_par_id ~ annotations ~ ("=" ~/ expr).? ~ ";"./).map {
      case (varType, varParId, ann, aff) => FZVarDecl[Any](varParId.ident, aff.map(Right(_)).getOrElse(Left(varType)), ann)
    }
  }

  //
  def constraint[_: P]: P[FZConstraint] =
    P("constraint" ~ pred_ann_id ~ "(" ~ expr.rep(sep = ",") ~ ")" ~ annotations ~ ";"./) map {
      case (predAnnId, expr, annotations) => FZConstraint(predAnnId, expr, annotations)
    }

  //
  def solve_goal[_: P]: P[FZSolve] =
    P("solve" ~ annotations).flatMap(ann =>
      P("satisfy" ~ ";").map { _ => FZSolve(Satisfy, ann) } |
        P("minimize" ~ expr ~ ";").map(expr => FZSolve(Minimize(expr), ann)) |
        P("maximize" ~ expr ~ ";").map(expr => FZSolve(Maximize(expr), ann))
    )

  //implicit def FZExpr2Scala[A](a: FZExpr[A]): A = a.value

}

