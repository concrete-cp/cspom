package cspom
package flatzinc

import java.io.InputStream

import scala.util.Try
import cspom.variable.CSPOMExpression


import fastparse.noApi._
import scala.io.Source
import fastparse.WhitespaceApi

object FlatZincFastParser extends CSPOM.Parser {

  //import WhitespaceApi._

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" \r\n\t").rep)
  }

  import White._

  def fzAnnotations(ann: Seq[FZAnnotation]): Seq[(String, Seq[FZAnnotation])] = {
    if (ann.isEmpty) {
      Seq()
    } else {
      Seq("fzAnnotations" -> ann)
    }
  }

  def apply(is: InputStream): Try[CSPOM] = {
    Try {
      flatzincModel.parse(Source.fromInputStream(is).mkString)
    }
      .flatMap {
        case Parsed.Success(cspom, _) => scala.util.Success(cspom)
        case Parsed.Failure(expected, index, extra) =>
          scala.util.Failure(new CSPParseException(s"expected: $expected, extra: $extra", null, index))
      }
  }

  /*
   * Definition of what's a flatzinc file : predicate(s) + parameter(s) + constraint(s) + solve goal
   */
  def flatzincModel = P(Start ~ pred_decl.rep ~/ (param_decl | var_decl).rep ~/ constraint.rep ~/ solve_goal ~ End).map {
    case (predicates, paramOrVar, constraints, goal) =>

      val params: Map[String, CSPOMExpression[Any]] = paramOrVar.collect {
        case FZParamDecl(name, expr) => name -> expr
      }.toMap

      val variables = paramOrVar.collect {
        case v: FZVarDecl[_] => v
      }

      assert(params.size + variables.size == paramOrVar.size)

      val declared = variables.foldLeft(params) {
        case (vars, FZVarDecl(name, expression, _)) => vars + (name -> expression.fold(_.genVariable, _.toCSPOM(vars)))
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
          (mode match {
            case Satisfy => WithParam(CSPOMGoal.Satisfy, params)
            case Minimize(e: FZExpr[Int]) =>
              val ce = e.toCSPOM(declared)
              WithParam(CSPOMGoal.Minimize(ce), params)
            case Maximize(e: FZExpr[Int]) =>
              WithParam(CSPOMGoal.Maximize(e.toCSPOM(declared)), params)
          })
        }
      }
  }

  val pred_ann_id: Parser[String] = P(CharIn('A' to 'Z', 'a' to 'z') ~~ CharIn('A' to 'Z', 'a' to 'z', '0' to '9', "_").repX).!
  //
  val var_par_id: Parser[FZVarParId[Any]] =
    P("_".repX ~~ CharIn('A' to 'Z', 'a' to 'z') ~~ CharIn('A' to 'Z', 'a' to 'z', '0' to '9', "_").repX).!.map { id =>
      FZVarParId[Any](id)
    }

  val bool_const: Parser[FZBoolConst] = P("true").map(_ => FZBoolConst(true)) |
    P("false").map(_ => FZBoolConst(false))

  val float_const: Parser[FZFloatConst] = P(
    CharIn("+-").? ~~ CharIn('0' to '9').repX(min = 1) ~~ "." ~~ CharIn('0' to '9').repX(min = 1) ~~ (
      CharIn("eE") ~~ CharIn("+-").? ~~ CharIn('0' to '9').repX(1)).?).!.map {
    f => println(f); FZFloatConst(f.toDouble)
  }

  val int_const: Parser[FZIntConst] = P(CharIn("+-").? ~~ CharIn('0' to '9').repX(min = 1)).!.map { i => FZIntConst(i.toInt) }

  val set_const: Parser[FZSetConst] = {
    P(int_const ~ ".." ~ int_const).map { case (i, j) => FZSetConst(i.value to j.value) } |
      P("{" ~ int_const.rep(sep = ",") ~ "}").map(i => FZSetConst(i map (_.value)))
  }

  val pred_decl: Parser[Any] = P("predicate" ~ pred_ann_id ~ "(" ~ pred_param.rep(sep = ",") ~ ")" ~ ";").map {
    case (a, p) => FZPredicate(a, p)
  }

  val index_set1: Parser[Option[Range]] =
    (P("1..") ~ int_const).map { i => Some(1 to i.value) } |
      P("int").map(_ => None)

  val index_set: Parser[Seq[Option[Range]]] = index_set1.rep(sep = ",")

  val single_var_type: Parser[FZVarType[_]] =
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

  val var_type: Parser[FZVarType[Any]] = {
    single_var_type |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ single_var_type).map {
        case (indices, vartype) => FZArray(indices, vartype)
      }
  }

  val var_pred_param_type: Parser[Any] =
    var_type |
      "var set of int" |
      "array" ~ "[" ~ index_set ~ "] of var set of int"
  //

  val par_type: P[FZVarType[_]] =
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

  val par_pred_param_type: Parser[Any] =
    par_type |
      float_const ~ ".." ~ float_const |
      int_const ~ ".." ~ int_const |
      P("{" ~ int_const.rep(sep = ",") ~ "}") |
      P("set" ~ "of" ~ int_const ~ ".." ~ int_const) |
      P("set" ~ "of" ~ "{" ~ int_const.rep(sep = ",") ~ "}") |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ float_const ~ ".." ~ float_const) |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ int_const ~ ".." ~ int_const) |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "{" ~ int_const.rep(sep = ",") ~ "}") |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "set" ~ "of" ~ int_const ~ ".." ~ int_const) |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "set" ~ "of" ~ int_const ~ ".." ~ int_const)

  val pred_param_type: Parser[Any] = par_pred_param_type | var_pred_param_type

  val pred_param: Parser[Any] = pred_param_type ~ ":" ~ pred_ann_id

  //

  val StringChars = !"\"\\".contains(_: Char)
  val escape = P("\\" ~ CharIn("\"/\\bfnrt"))
  val strChars = P(CharsWhile(StringChars))

  val stringLiteral: Parser[String] =
    P("\"" ~/ (strChars | escape).rep.! ~ "\"")

  lazy val array_expr: Parser[FZArrayExpr[Any]] = {
    P("[" ~ expr.rep(sep = "," ~/ Pass) ~ "]").map { l => FZArrayExpr(l) }
  }

  lazy val array_exprInAnn: Parser[FZArrayExpr[Any]] = {
    P("[" ~ exprInAnn.rep(sep = ",") ~ "]").map { l => FZArrayExpr(l) }
  }

  val expr: Parser[FZExpr[Any]] = {
    bool_const |
      set_const |
      int_const |
      float_const |
      (var_par_id ~ "[" ~ int_const ~ "]").map {
        case (varParId, index) => varParId.index(index.value)
      } |
      var_par_id |
      array_expr
  }

  //
  val annotation: Parser[FZAnnotation] = {
    P(pred_ann_id ~ ("(" ~ exprInAnn.rep(sep = ",") ~ ")").?).map {
      case (id, expressions) => FZAnnotation(id, expressions.toSeq.flatten)
    }
  }

  val annotations: Parser[Seq[FZAnnotation]] = P("::" ~ annotation).rep

  //
  val exprInAnn: Parser[FZExpr[Any]] = P(

    array_exprInAnn |
      annotation |
      expr |
      stringLiteral.map(FZStringLiteral))

  val param_decl: Parser[FZParamDecl[Any]] = P(par_type ~ ":" ~ var_par_id ~ "=" ~ expr ~ ";").map {
    case (t, id, expr) =>
      val e: CSPOMExpression[_] = (t, expr) match {
        case (FZArray(Seq(indices), typ), a: FZArrayExpr[_]) => a.asConstant(indices.get)
        case (_, c: FZConstant[_]) => c.asConstant
        case _ => throw new IllegalArgumentException("Constant expected")
      }

      FZParamDecl(id.ident, e)
  }

  val var_decl: Parser[FZVarDecl[Any]] = {
    P(var_type ~ ":" ~ var_par_id ~ annotations ~ ("=" ~ expr).? ~ ";").map {
      case (varType, varParId, ann, aff) => FZVarDecl[Any](varParId.ident, aff.map(Right(_)).getOrElse(Left(varType)), ann)
    }

  }
  //
  val constraint: Parser[FZConstraint] =
    P("constraint" ~ pred_ann_id ~ "(" ~ expr.rep(sep = ",") ~ ")" ~ annotations ~ ";") map {
      case (predAnnId, expr, annotations) => FZConstraint(predAnnId, expr, annotations)
      //      
      //    }
    }
  //
  val solve_goal: Parser[FZSolve] =
    P("solve" ~ annotations ~ "satisfy" ~ ";").map { ann => FZSolve(Satisfy, ann) } |
      P("solve" ~ annotations ~ "minimize" ~ expr ~ ";").map { case (ann, expr) => FZSolve(Minimize(expr), ann) } |
      P("solve" ~ annotations ~ "maximize" ~ expr ~ ";").map { case (ann, expr) => FZSolve(Maximize(expr), ann) }

  //implicit def FZExpr2Scala[A](a: FZExpr[A]): A = a.value

}

