package cspom.flatzinc

import java.io.InputStream
import java.io.InputStreamReader
import scala.collection.immutable.PagedSeq
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.PagedSeqReader
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.CSPParseException
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.compiler.ConstraintCompiler
import cspom.variable.SimpleExpression

sealed trait FZDecl
case class FZParamDecl(name: String, expression: CSPOMExpression[_]) extends FZDecl
case class FZVarDecl(name: String, expression: CSPOMExpression[_], affectation: Option[FZExpr[_]], annotations: Seq[FZAnnotation])

object FlatZincParser extends RegexParsers with CSPOM.Parser {

  def fzAnnotations(ann: Seq[FZAnnotation]): Seq[(String, Seq[FZAnnotation])] = {
    if (ann.isEmpty) {
      Seq()
    } else {
      Seq("fzAnnotations" -> ann)
    }
  }

  def apply(is: InputStream): Try[(CSPOM, Map[Symbol, Any])] = Try {
    val jreader = new InputStreamReader(is)
    val sreader = new PagedSeqReader(PagedSeq.fromReader(jreader))

    flatzincModel(sreader)
  }
    .flatMap {
      case Success((cspom, goal), _) => util.Success((cspom, Map('goal -> goal)))
      case n: NoSuccess              => util.Failure(new CSPParseException(n.toString, null, n.next.pos.line))
    }

  private def mapVariables(params: Map[String, CSPOMExpression[_]],
                           variables: Seq[FZVarDecl]) = {

    val decl: Map[String, CSPOMExpression[Any]] = variables
      .map {
        case FZVarDecl(name, e, _, _) => name -> e
      }
      .toMap

    // /!\ do not use Map to avoid non-determinism between runs
    val affectations: Seq[(CSPOMExpression[_], CSPOMExpression[_])] = variables.collect {
      case FZVarDecl(_, expr, Some(aff), _) => expr -> aff.toCSPOM(decl)
    }

    val annotations: Map[String, Seq[FZAnnotation]] = variables.map {
      case FZVarDecl(name, _, _, ann) => name -> ann
    }
      .toMap

    (params ++ decl, affectations, annotations)
  }

  /*
   * Definition of what's a flatzinc file : predicate(s) + parameter(s) + constraint(s) + solve goal
   */
  def flatzincModel: Parser[(CSPOM, FZSolve)] = rep(pred_decl) ~ rep(param_decl | var_decl) >> {
    case predicates ~ paramOrVar =>
      val params: Map[String, CSPOMExpression[_]] = paramOrVar.collect {
        case FZParamDecl(name, expr) => name -> expr
      }.toMap
      val variables = paramOrVar.collect {
        case v: FZVarDecl => v
      }
      assert(params.size + variables.size == paramOrVar.size)
      val (declared, affectations, annotations) = mapVariables(params, variables)
      success((declared, affectations, annotations)) ~ rep(constraint(declared)) ~ solve_goal
  } ^^ {
    case (declared, affectations, annotations) ~ constraints ~ goal =>
      val p = CSPOM { implicit problem =>

        for ((name, expr) <- declared) {
          expr as name
          for (
            annotation <- annotations.get(name);
            (title, ann) <- fzAnnotations(annotation)
          ) {
            problem.addAnnotation(name, title, ann)
          }

        }

        for (c <- constraints) {
          CSPOM.ctr(c)
        }

        for ((e: CSPOMExpression[Any], a: CSPOMExpression[Any]) <- affectations) {
          ConstraintCompiler.deepReplace(e, a, problem)

          //CSPOM.ctr(CSPOMConstraint('eq)(e, a))
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
      "set of int" ^^^ FZIntSet |
      ("array" ~ "[") ~> index_set <~ "] of bool" ^^ { FZArray[Boolean](_, FZBoolean) } |
      ("array" ~ "[") ~> index_set <~ "] of float" ^^ { s => FZArray[Double](s, FZFloat) } |
      ("array" ~ "[") ~> index_set <~ "] of int" ^^ { FZArray[Int](_, FZInt) } |
      ("array" ~ "[") ~> index_set <~ "] of set of int" ^^ { FZArray[Int](_, FZIntSet) }
  }

  def par_pred_param_type: Parser[Any] =
    par_type |
      float_const ~ ".." ~ float_const |
      int_const ~ ".." ~ int_const |
      "{" ~ repsep(int_const, ",") ~ "}" |
      "set of " ~ int_const ~ ".." ~ int_const |
      "set of {" ~ repsep(int_const, ",") ~ "}" |
      ("array" ~ "[") ~ index_set ~ "] of " ~ float_const ~ ".." ~ float_const |
      ("array" ~ "[") ~ index_set ~ "] of " ~ int_const ~ ".." ~ int_const |
      ("array" ~ "[") ~ index_set ~ "] of " ~ "{" ~ repsep(int_const, ",") ~ "}" |
      ("array" ~ "[") ~ index_set ~ "] of set of " ~ int_const ~ ".." ~ int_const |
      ("array" ~ "[") ~ index_set ~ "] of set of " ~ int_const ~ ".." ~ int_const

  def var_type: Parser[FZVarType[_]] = {
    single_var_type |
      (("array" ~ "[") ~> index_set <~ "] of ") ~ single_var_type ^^ {
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
      "array" ~ "[" ~ index_set ~ "] of var set of int"

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

  def var_par_id: Parser[FZVarParId[Any]] = """_*[A-Za-z][A-Za-z0-9_]*""".r ^^ { id => FZVarParId[Any](id) }

  def bool_const: Parser[FZBoolConst] = "true" ^^^ FZBoolConst(true) |
    "false" ^^^ FZBoolConst(false)

  def float_const: Parser[FZFloatConst] = """[+-]?[0-9][0-9]*\.[0-9][0-9]*[[eE][+-]?[0-9][0-9]*]?""".r ^^ {
    f => FZFloatConst(f.toDouble)
  }

  def int_const: Parser[FZIntConst] = """[+-]?[0-9][0-9]*""".r ^^ (i => FZIntConst(i.toInt))

  def set_const: Parser[FZSetConst] = {
    (int_const <~ "..") ~ int_const ^^ { case i ~ j => FZSetConst(i.value to j.value) } |
      "{" ~> repsep(int_const, ",") <~ "}" ^^ (i => FZSetConst(i map (_.value)))
  }

  def array_expr: Parser[FZArrayExpr[Any]] = {
    "[" ~> repsep(expr, ",") <~ "]" ^^ { l => FZArrayExpr(l) }
  }

  def array_exprInAnn: Parser[FZArrayExpr[Any]] = {
    "[" ~> repsep(exprInAnn, ",") <~ "]" ^^ { l => FZArrayExpr(l) }
  }

  def param_decl: Parser[FZParamDecl] = (par_type <~ ":") ~ (var_par_id <~ "=") ~ expr <~ ";" ^^ {
    case t ~ id ~ expr =>
      val e: CSPOMExpression[_] = (t, expr) match {
        case (FZArray(Seq(indices), typ), a: FZArrayExpr[_]) => a.asConstant(indices.range)
        case (_, c: FZConstant[_]) => c.asConstant
        case _ => throw new IllegalArgumentException("Constant expected")
      }

      FZParamDecl(id.ident, e)
  }

  def var_decl: Parser[FZVarDecl] = {
    (var_type <~ ":") ~ var_par_id ~ annotations ~ opt("=" ~> expr) <~ ";" ^^ {
      case varType ~ varParId ~ ann ~ aff => FZVarDecl(varParId.ident, varType.genVariable, aff, ann)
    }

  }

  def constraint(declared: Map[String, CSPOMExpression[Any]]): Parser[CSPOMConstraint[Boolean]] =
    {
      "constraint" ~> pred_ann_id ~ ("(" ~> repsep(expr, ",") <~ ")") ~ annotations <~ ";" ^^ {
        case predAnnId ~ expr ~ annotations =>
          CSPOMConstraint(Symbol(predAnnId))(expr.map(_.toCSPOM(declared)): _*) withParam
            (fzAnnotations(annotations): _*)
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