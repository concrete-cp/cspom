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
import cspom.CSPParseException
import cspom.compiler.ProblemCompiler
import cspom.Loggable
import CSPOM._

trait DebugJavaTokenParsers extends JavaTokenParsers {
  private val DEBUG = false

  class Wrap[+T](name: String, parser: Parser[T]) extends Parser[T] with Loggable {
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

object FlatZincParser extends DebugJavaTokenParsers {

  def parse(is: InputStream): (CSPOM, Map[Symbol, Any]) = {
    val reader = new InputStreamReader(is)
    flatzincModel(StreamReader(reader)) match {
      case Success((cspom, goal), _) =>
        ProblemCompiler.compile(cspom, FZPatterns())
        (cspom, Map('goal -> goal))
      case NoSuccess(msg, next) => throw new CSPParseException(msg, null, next.pos.line)
    }
  }

  private def mapVariables(variables: Seq[(String, CSPOMExpression, Option[FZExpr[_]])]) = {
    val varMap = variables.collect {
      case (name, single: CSPOMVariable, _) => name -> single
    } toMap

    val seqMap = variables.collect {
      case (name, seq: CSPOMSeq[CSPOMExpression], _) => name -> seq
    } toMap

    val affectations = variables.collect {
      case (_, expr, Some(aff)) => expr -> aff.toCSPOM(varMap, seqMap)
    }

    (varMap, seqMap, affectations)
  }

  /*
   * Definition of what's a flatzinc file : predicate(s) + parameter(s) + constraint(s) + solve goal
   */
  def flatzincModel: Parser[(CSPOM, Any)] = rep(pred_decl) ~ rep(param_decl) ~ rep(var_decl) >> {
    case predicates ~ parameters ~ variables =>
      val (varMap, seqMap, affectations) = mapVariables(variables)
      success(varMap, seqMap, affectations) ~ rep(constraint(varMap, seqMap)) ~ solve_goal
  } ^^ {
    case (varMap, seqMap, affectations) ~ constraints ~ goal =>
      val p = CSPOM {
        for ((name, expr) <- varMap.iterator ++ seqMap) {
          expr as name
        }
        for (c <- constraints) {
          ctr(c)
        }
        for ((e, a) <- affectations) {
          ctr(new CSPOMConstraint('eq, Seq(e, a)))
        }
      }
      (p, goal)
    //      val p = new CSPOM()
    //      //val allVars = (varMap.values ++ seqMap.values.flatMap(_.flattenVariables))
    //      for ((name, expr) <- varMap.iterator ++ seqMap) {
    //        p.nameExpression(expr, name)
    //      }
    //      constraints.foreach(p.ctr)
    //      p
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

  def index_set: Parser[IndexSet] =
    "1.." ~> int_const ^^ { FZRange(_) } |
      "int" ^^^ SomeIndexSet

  def expr: Parser[FZExpr[_]] = "expr" !!! {
    bool_const |
      set_const |
      int_const |
      float_const |
      var_par_id ~ ("[" ~> int_const <~ "]") ^^ {
        case varParId ~ index => varParId.index(index)
      } |
      var_par_id |
      array_expr
  }

  def exprInAnn: Parser[FZExpr[_]] =
    bool_const |
      set_const |
      int_const |
      float_const |
      var_par_id ~ ("[" ~> int_const <~ "]") ^^ {
        case varParId ~ index => varParId.index(index)
      } |
      var_par_id |||
      array_exprInAnn |||
      annotation |
      stringLiteral ^^ FZStringLiteral

  //  def cspomExpr(vars: Map[String, CSPOMVariable], seqs: Map[String, CSPOMSeq[CSPOMExpression]]): Parser[CSPOMExpression] =
  //    bool_const ^^ { _.toCSPOM(vars, seqs) } |
  //      int_const ^^ { _.toCSPOM(vars, seqs) } |
  //      float_const ^^ { _.toCSPOM(vars, seqs) } |
  //      var_par_id ~ ("[" ~> int_const <~ "]") ^^ { case id ~ index => id.index(index).toCSPOM(vars, seqs) } |
  //      var_par_id ^^ { _.toCSPOM(vars, seqs) } |
  //      "[" ~> repsep(cspomExpr(vars, seqs), ",") <~ "]" ^^ { case seq => new CSPOMSeq(seq) }

  def pred_ann_id: Parser[String] = ident //"[A-Za-z][A-Za-z0-9_]*".r

  def var_par_id: Parser[FZVarParId] = ident ^^ FZVarParId //"_*[A-Za-z][A-Za-z0-9_]*".r

  def bool_const: Parser[FZBoolConst] = "true" ^^^ FZBoolConst(true) | "false" ^^^ FZBoolConst(false)

  def float_const: Parser[FZFloatConst] = floatingPointNumber ^^ (f => FZFloatConst(f.toDouble))

  def int_const: Parser[FZIntConst] = wholeNumber ^^ (i => FZIntConst(i.toInt))

  def set_const: Parser[FZSetConst] = "set_const" !!! {
    int_const ~ ".." ~ int_const ^^ { case i ~ ".." ~ j => FZSetConst(i.value to j.value) } |
      "{" ~> repsep(int_const, ",") <~ "}" ^^ (i => FZSetConst(i map (_.value)))
  }

  def array_expr: Parser[FZArrayExpr[_]] = "array_expr" !!! {
    "[" ~> repsep(expr, ",") <~ "]" ^^ { case l: Seq[FZExpr[_]] => FZArrayExpr(l) }
  }
  
    def array_exprInAnn: Parser[FZArrayExpr[_]] = "array_expr" !!! {
    "[" ~> repsep(exprInAnn, ",") <~ "]" ^^ { case l: Seq[FZExpr[_]] => FZArrayExpr(l) }
  }

  def param_decl: Parser[Parameter] = par_type ~ ":" ~ var_par_id ~ "=" ~ expr <~ ";" ^^ {
    case t ~ ":" ~ id ~ "=" ~ expr => Parameter.getParamater(id, "int")
  }

  def var_decl: Parser[(String, CSPOMExpression, Option[FZExpr[_]])] = "var_decl" !!! {
    var_type ~ ":" ~ var_par_id ~ annotations ~ opt("=" ~> expr) <~ ";" ^^ {
      case varType ~ ":" ~ varParId ~ ann ~ aff => (varParId, varType.genVariable(ann), aff)
    }

  }

  def constraint(vars: Map[String, CSPOMVariable], seqs: Map[String, CSPOMSeq[CSPOMExpression]]): Parser[CSPOMConstraint] =
    "constraint" !!! {
      "constraint" ~> pred_ann_id ~ "(" ~ repsep(expr, ",") ~ ")" ~ annotations ~ ";" ^^ {
        case predAnnId ~ "(" ~ expr ~ ")" ~ annotations ~ ";" =>
          new CSPOMConstraint(Symbol(predAnnId), expr.map(_.toCSPOM(vars, seqs)), Map("fzAnnotations" -> annotations))
      }
    }

  def solve_goal: Parser[Any] =
    "solve" ~ annotations ~ "satisfy" ~ ";" |
      "solve" ~ annotations ~ "minimize" ~ expr ~ ";" |
      "solve" ~ annotations ~ "maximize" ~ expr ~ ";"

  def annotations: Parser[Seq[FZAnnotation]] = "annotations" !!! rep("::" ~> annotation)

  def annotation: Parser[FZAnnotation] = "annotation" !!! {
    pred_ann_id ~ ("(" ~> repsep(exprInAnn, ",") <~ ")") ^^ {
      case id ~ expressions => FZAnnotation(id, expressions)
    } |
      pred_ann_id ^^ (FZAnnotation(_, Seq()))
  }

  implicit def FZExpr2Scala[A](a: FZExpr[A]): A = a.value

}