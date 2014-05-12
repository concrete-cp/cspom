package cspom.flatzinc

import java.io.InputStream
import java.io.InputStreamReader
import java.text.ParseException

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.StreamReader

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.CSPParseException
import cspom.StatisticsManager
import cspom.compiler.ProblemCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable

object FlatZincParser extends RegexParsers {

  def fzAnnotations(ann: Seq[FZAnnotation]): Map[String, Seq[FZAnnotation]] = {
    if (ann.isEmpty) {
      Map()
    } else {
      Map("fzAnnotations" -> ann)
    }
  }

  def parse(is: InputStream): (CSPOM, Map[Symbol, Any]) = {
    val reader = new InputStreamReader(is)

    val (parseResult, time) = StatisticsManager.time(flatzincModel(StreamReader(reader)))

    parseResult match {
      case Success((cspom, goal), _) =>
        ProblemCompiler.compile(cspom, FZPatterns())
        (cspom, Map('goal -> goal))
      case NoSuccess(msg, next) => throw new CSPParseException(msg, null, next.pos.line)
    }
  }

  private def mapVariables(params: Map[String, CSPOMExpression[_]], variables: Seq[(String, CSPOMExpression[_], Option[FZExpr[_]])]) = {

    val decl: Map[String, CSPOMExpression[Any]] = variables.map {
      case (name, single: CSPOMVariable[_], _) => name -> single
      case (name, seq: CSPOMSeq[_], _) => name -> seq
      case (name, c: CSPOMConstant[_], _) =>
        throw new IllegalStateException(s"Unexpected constant $name: $c")
    } toMap

    val affectations: Seq[(CSPOMExpression[_], CSPOMExpression[_])] = variables.collect {
      case (_, expr, Some(aff)) => expr -> aff.toCSPOM(decl)
    }

    (params ++ decl, affectations)
  }

  /*
   * Definition of what's a flatzinc file : predicate(s) + parameter(s) + constraint(s) + solve goal
   */
  def flatzincModel: Parser[(CSPOM, FZSolve)] = rep(pred_decl) ~ rep(param_decl) ~ rep(var_decl) >> {
    case predicates ~ parameters ~ variables =>
      val params = parameters.toMap
      val (declared, affectations) = mapVariables(params, variables)
      success((declared, affectations)) ~ rep(constraint(declared)) ~ solve_goal
  } ^^ {
    case (declared, affectations) ~ constraints ~ goal =>
      val p = CSPOM { implicit problem =>

        for ((name, expr) <- declared) {
          expr as name
        }
        for (c <- constraints) {
          CSPOM.ctr(c)
        }

        for ((e: CSPOMExpression[Any], a: CSPOMExpression[Any]) <- affectations) {
          CSPOM.ctr(CSPOMConstraint('eq, Seq(e, a)))
        }
      }
      (p, goal)
  }

  def pred_decl: Parser[Any] = "predicate" ~> pred_ann_id ~ "(" ~ repsep(pred_param, ",") ~ ")" <~ ";"

  def pred_param: Parser[Any] = pred_param_type ~ ":" ~ pred_ann_id

  def pred_param_type: Parser[Any] = par_pred_param_type | var_pred_param_type

  def par_type: Parser[FZVarType[_]] = {
    "bool" ^^^ FZBoolean |
      "float" ^^^ FZFloat |
      "int" ^^^ FZInt |
      "set of int" ~> err("Unsupported") |
      "array [" ~> index_set <~ "] of bool" ^^ { FZArray[Boolean](_, FZBoolean) } |
      "array [" ~> index_set <~ "] of float" ^^ { s => FZArray[Double](s, FZFloat) } |
      "array [" ~> index_set <~ "] of int" ^^ { FZArray[Int](_, FZInt) } |
      "array [" ~> index_set <~ "] of set of int" ^^ { FZArray[Int](_, FZIntSet) }
  }

  def par_pred_param_type: Parser[Any] =
    par_type |
      float_const ~ ".." ~ float_const |
      int_const ~ ".." ~ int_const |
      "{" ~ repsep(int_const, ",") ~ "}" |
      "set of " ~ int_const ~ ".." ~ int_const |
      "set of {" ~ repsep(int_const, ",") ~ "}" |
      "array [" ~ index_set ~ "] of " ~ float_const ~ ".." ~ float_const |
      "array [" ~ index_set ~ "] of " ~ int_const ~ ".." ~ int_const |
      "array [" ~ index_set ~ "] of " ~ "{" ~ repsep(int_const, ",") ~ "}" |
      "array [" ~ index_set ~ "] of set of " ~ int_const ~ ".." ~ int_const |
      "array [" ~ index_set ~ "] of set of " ~ int_const ~ ".." ~ int_const

  def var_type: Parser[FZVarType[_]] = {
    single_var_type |
      ("array [" ~> index_set <~ "] of ") ~ single_var_type ^^ {
        case indices ~ vartype => FZArray(indices, vartype)
      }
  }

  def single_var_type: Parser[FZVarType[_]] =
    "var bool" ^^^ FZBoolean |
      "var float" ^^^ FZFloat |
      ("var" ~> float_const <~ "..") ~ float_const ^^ {
        case lb ~ ub => FZFloatInterval(lb.value, ub.value)
      } |
      "var int" ^^^ FZInt |
      ("var" ~> int_const <~ "..") ~ int_const ^^ {
        case lb ~ ub => FZIntInterval(lb.value, ub.value)
      } |
      "var" ~> "{" ~> repsep(int_const, ",") <~ "}" ^^ {
        case list => FZIntSeq(list.map(_.value))
      } |
      "var set of" ~> int_const ~ ".." ~ int_const ~> err("Unsupported domain type") |
      "var set of" ~> "{" ~> repsep(int_const, ",") ~ "}" ~> err("Unsupported domain type")

  def var_pred_param_type: Parser[Any] =
    var_type |
      "var set of int" |
      "array [" ~ index_set ~ "] of var set of int"

  def index_set: Parser[Seq[IndexSet]] = rep1sep(index_set1, ",")

  def index_set1: Parser[IndexSet] =
    "1.." ~> int_const ^^ { i => FZRange(1 to i.value) } |
      "int" ^^^ SomeIndexSet

  def expr: Parser[FZExpr[Any]] = {
    bool_const |
      set_const |
      float_const |
      int_const |
      var_par_id ~ ("[" ~> int_const <~ "]") ^^ {
        case varParId ~ index => varParId.index(index.value)
      } |
      var_par_id |
      array_expr
  }

  def exprInAnn: Parser[FZExpr[Any]] =
    expr |||
      array_exprInAnn |||
      annotation |
      stringLiteral ^^ FZStringLiteral

  def stringLiteral: Parser[String] = ("\"" + """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""" + "\"").r

  def pred_ann_id: Parser[String] = """[A-Za-z][A-Za-z0-9_]*""".r

  def var_par_id: Parser[FZVarParId] = """_*[A-Za-z][A-Za-z0-9_]*""".r ^^ FZVarParId

  def bool_const: Parser[FZBoolConst] = "true" ^^^ FZBoolConst(true) | "false" ^^^ FZBoolConst(false)

  def float_const: Parser[FZFloatConst] = """[+-]?[0-9][0-9]*\.[0-9][0-9]*[[eE][+-]?[0-9][0-9]*]?""".r ^^ {
    f => FZFloatConst(f.toDouble)
  }

  def int_const: Parser[FZIntConst] = """[+-]?[0-9][0-9]*""".r ^^ (i => FZIntConst(i.toInt))

  def set_const: Parser[FZSetConst] = {
    int_const ~ ".." ~ int_const ^^ { case i ~ ".." ~ j => FZSetConst(i.value to j.value) } |
      "{" ~> repsep(int_const, ",") <~ "}" ^^ (i => FZSetConst(i map (_.value)))
  }

  def array_expr: Parser[FZArrayExpr[Any]] = {
    "[" ~> repsep(expr, ",") <~ "]" ^^ {
      case l: Seq[FZExpr[Any]] => FZArrayExpr(l)
    }
  }

  def array_exprInAnn: Parser[FZArrayExpr[Any]] = {
    "[" ~> repsep(exprInAnn, ",") <~ "]" ^^ { case l: Seq[FZExpr[Any]] => FZArrayExpr(l) }
  }

  def param_decl: Parser[(String, CSPOMExpression[_])] = (par_type <~ ":") ~ (var_par_id <~ "=") ~ expr <~ ";" ^^ {
    case t ~ id ~ expr =>
      val e: CSPOMExpression[_] = (t, expr) match {
        case (FZArray(Seq(indices), typ), a: FZArrayExpr[_]) => a.asConstant(indices.range)
        case (_, c: FZConstant[_]) => c.asConstant
        case _ => throw new IllegalArgumentException("Constant expected")
      }

      id.value -> e
  }

  def var_decl: Parser[(String, CSPOMExpression[_], Option[FZExpr[_]])] = {
    var_type ~ ":" ~ var_par_id ~ annotations ~ opt("=" ~> expr) <~ ";" ^^ {
      case varType ~ ":" ~ varParId ~ ann ~ aff => (varParId.value, varType.genVariable(ann), aff)
    }

  }

  def constraint(declared: Map[String, CSPOMExpression[Any]]): Parser[CSPOMConstraint[Boolean]] =
    {
      "constraint" ~> pred_ann_id ~ ("(" ~> repsep(expr, ",") <~ ")") ~ annotations <~ ";" ^^ {
        case predAnnId ~ expr ~ annotations =>
          CSPOMConstraint(
            Symbol(predAnnId),
            expr.map(_.toCSPOM(declared)),
            fzAnnotations(annotations))
      }
    }

  def solve_goal: Parser[FZSolve] =
    "solve" ~> annotations <~ "satisfy" ~ ";" ^^ { ann => FZSolve(Satisfy, ann) } |
      "solve" ~> annotations ~ ("minimize" ~> expr <~ ";") ^^ { case ann ~ expr => FZSolve(Minimize(expr), ann) } |
      "solve" ~> annotations ~ ("maximize" ~> expr <~ ";") ^^ { case ann ~ expr => FZSolve(Maximize(expr), ann) }

  def annotations: Parser[Seq[FZAnnotation]] = rep("::" ~> annotation)

  def annotation: Parser[FZAnnotation] = {
    pred_ann_id ~ ("(" ~> repsep(exprInAnn, ",") <~ ")") ^^ {
      case id ~ expressions => FZAnnotation(id, expressions)
    } |
      pred_ann_id ^^ (FZAnnotation(_, Seq()))
  }

  //implicit def FZExpr2Scala[A](a: FZExpr[A]): A = a.value

}