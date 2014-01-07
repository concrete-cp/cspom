package cspom.flatzinc

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.RegexParsers
import cspom.variable.CSPOMVariable
import cspom.variable.IntInterval
import cspom.CSPOM
import java.io.InputStream
import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader
import cspom.CSPOMConstraint
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMFalse
import cspom.variable.DoubleConstant
import cspom.variable.IntConstant
import cspom.variable.CSPOMSeq
import scala.util.parsing.input.CharSequenceReader

trait DebugJavaTokenParsers extends JavaTokenParsers {
  private val DEBUG = false

  class Wrap[+T](name: String, parser: Parser[T]) extends Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val offset = in.offset
      val t = parser.apply(in)
      println(name + ".apply for token " + first +
        " at position " + pos + " offset " + offset + " returns " + t)
      t
    }
  }

  implicit def toWrapped(name: String) = new {
    def !!![T](p: Parser[T]) = if (DEBUG) new Wrap(name, p) else p
  }
}

object FlatzincDSL extends DebugJavaTokenParsers {

  def parse(is: InputStreamReader) = {
    flatzincModel(StreamReader(is))
  }

  private def mapVariables(variables: Seq[CSPOMExpression]) = {
    val varMap = variables.collect {
      case single: CSPOMVariable =>
        single.name -> single
    } toMap

    val seqMap = variables.collect {
      case seq: CSPOMSeq[CSPOMExpression] => seq.name -> seq
    } toMap

    (varMap, seqMap)
  }

  /*
   * Definition of what's a flatzinc file : predicate(s) + parameter(s) + constraint(s) + solve goal
   */
  def flatzincModel = rep(pred_decl) ~ rep(param_decl) ~ rep(var_decl) >> {
    case predicates ~ parameters ~ variables =>
      val (varMap, seqMap) = mapVariables(variables)
      success(varMap, seqMap) ~ rep(constraint(varMap, seqMap)) ~ solve_goal
  } ^^ {
    case (varMap, seqMap) ~ constraints ~ goal =>
      val p = new CSPOM()
      val allVars = (varMap.values ++ seqMap.values.flatMap(_.flattenVariables)).toSet
      //allVars.foreach(p.addVariable)
      constraints.foreach(p.ctr)
      p
  }

  def pred_decl: Parser[Any] = "predicate" ~> ident ~ "(" ~ repsep(pred_param, ",") ~ ")" ~ ";"

  def pred_param: Parser[Any] = pred_param_type ~ ":" ~ pred_ann_id

  def pred_param_type: Parser[Any] = par_pred_param_type | var_pred_param_type

  def par_type: Parser[Any] =
    "bool" |
      "float" |
      "int" |
      "set of int" |
      "array [" ~ index_set ~ "] of bool" |
      "array [" ~ index_set ~ "] of float" |
      "array [" ~ index_set ~ "] of int" |
      "array [" ~ index_set ~ "] of set of int"

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
  //    "true" ^^ (_.toBoolean) |
  //      "false" ^^ (_.toBoolean) |
  //      float_const ^^ (_.toDouble) |
  //      "set of int" |
  //      int_const ^^ (_.toInt) |
  //      "array [" ~ index_set ~ "] of bool" |
  //      "array [" ~ index_set ~ "] of int" |
  //      "array [" ~ index_set ~ "] of float" |
  //      "set of int"

  def var_type: Parser[FZVarType] = "var_type" !!! {
    single_var_type |
      ("array [" ~> index_set <~ "] of ") ~ single_var_type ^^ {
        case indices ~ vartype => FZArray(indices, vartype)
      }
  }

  def single_var_type: Parser[FZVarType] =
    "var bool" ^^^ FZBoolean |
      "var float" ^^^ FZFloat |
      ("var" ~> float_const <~ "..") ~ float_const ^^ {
        case lb ~ ub => FZFloatInterval(lb, ub)
      } |
      "var int" ^^^ FZInt |
      ("var" ~> int_const <~ "..") ~ int_const ^^ {
        case lb ~ ub => FZIntInterval(lb, ub)
      } |
      "var" ~> "{" ~> repsep(int_const, ",") <~ "}" ^^ {
        case list => FZIntSeq(list)
      } |
      "var set of" ~> int_const ~ ".." ~ int_const ~> err("Unsupported domain type") |
      "var set of" ~> "{" ~> repsep(int_const, ",") ~ "}" ~> err("Unsupported domain type")

  def var_pred_param_type: Parser[Any] =
    var_type |
      "var set of int" |
      "array [" ~ index_set ~ "] of var set of int"

  def index_set: Parser[Range] =
    int_const ~ ".." ~ int_const ^^ { case i ~ ".." ~ j => i to j } |
      int_const ^^ { i => i to i }

  /**
   *   TODO : vérifier la BNF de fzn, il semble que l'erreur rencontrée vienne du fait
   *   que l'annotation ne soit pas confirme à la BNF
   */
  def expr: Parser[Any] = "expr" !!! {
    bool_const |
      set_const |
      float_const |
      int_const |
      var_par_id ~ "[" ~ int_const ~ "]" |
      var_par_id |
      array_expr |
      annotation |
      stringLiteral
  }

  def cspomExpr(vars: Map[String, CSPOMVariable], seqs: Map[String, CSPOMSeq[CSPOMExpression]]): Parser[CSPOMExpression] =
    bool_const ^^ { case true => CSPOMTrue; case false => CSPOMFalse } |
      int_const ^^ { case value => IntConstant(value) } |
      float_const ^^ { case value => DoubleConstant(value) } |
      var_par_id ~ ("[" ~> int_const <~ "]") ^^ { case id ~ index => seqs(id)(index) } |
      var_par_id ^^ { case id => vars.getOrElse(id, seqs(id)) } |
      "[" ~> repsep(cspomExpr(vars, seqs), ",") <~ "]" ^^ { case seq => new CSPOMSeq(seq) }

  def pred_ann_id: Parser[String] = ident //"[A-Za-z][A-Za-z0-9_]*".r

  def var_par_id: Parser[String] = ident //"_*[A-Za-z][A-Za-z0-9_]*".r

  def bool_const: Parser[Boolean] = ("true" | "false") ^^ (_.toBoolean)

  def float_const: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def int_const: Parser[Int] = wholeNumber ^^ (_.toInt)

  def set_const: Parser[Seq[Int]] = "set_const" !!! {
    int_const ~ ".." ~ int_const ^^ { case i ~ ".." ~ j => i to j } |
      "{" ~> repsep(int_const, ",") <~ "}"
  }

  def array_expr: Parser[Seq[Any]] = "array_expr" !!! {
    "[" ~> repsep(expr, ",") <~ "]"
  }

  def param_decl: Parser[Parameter] = par_type ~ ":" ~ var_par_id ~ "=" ~ expr <~ ";" ^^ {
    case t ~ ":" ~ id ~ "=" ~ expr => Parameter.getParamater(id, "int")
  }

  def var_decl: Parser[CSPOMExpression] = "var_decl" !!! {
    var_type ~ ":" ~ var_par_id ~ annotations ~ opt("=" ~ expr) <~ ";" ^? ({
      case varType ~ ":" ~ varParId ~ ann ~ None => varType.genVariable(varParId, ann.toSet)
    }, _ => "Expressions not supported in var declaration")

  }
  //  ^^
  //    { case t ~ ":" ~ id ~ annot ~ _ ~ ";" => Variable.getVariable(id, "D0").toXML }

  def constraint(vars: Map[String, CSPOMVariable], seqs: Map[String, CSPOMSeq[CSPOMExpression]]): Parser[CSPOMConstraint] =
    "constraint" !!! {
      "constraint" ~> pred_ann_id ~ "(" ~ repsep(cspomExpr(vars, seqs), ",") ~ ")" ~ annotations ~ ";" ^^ {
        case predAnnId ~ "(" ~ expr ~ ")" ~ annotations ~ ";" =>
          new CSPOMConstraint(Symbol(predAnnId), expr, annotations.map(_ -> Unit).toMap)
      }
    }

  def solve_goal: Parser[Any] =
    "solve" ~ annotations ~ "satisfy" ~ ";" |
      "solve" ~ annotations ~ "minimize" ~ expr ~ ";" |
      "solve" ~ annotations ~ "maximize" ~ expr ~ ";"

  def annotations: Parser[Seq[String]] = "annotations" !!! rep("::" ~> annotation)

  def annotation: Parser[String] = "annotation" !!! {
    pred_ann_id ~ "(" ~ repsep(expr, ",") ~ ")" ^^ (_.toString) |
      pred_ann_id ^^ (_.toString)
  }

}