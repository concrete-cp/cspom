package cspom.flatzinc

import fastparse.noApi._
import fastparse.WhitespaceApi

object ParserExample extends App {

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" \r\n\t").rep)
  }

  import White._

  val model = P(Start ~ (param_decl | var_decl).rep ~ End)

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
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "bool").map { FZArray[Boolean](_, FZBoolean) } |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "float").map { s => FZArray[Double](s, FZFloat) } |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "int").map { FZArray[Int](_, FZInt) } |
      P("array" ~ "[" ~ index_set ~ "]" ~ "of" ~ "set" ~ "of" ~ "int").map { FZArray[Int](_, FZIntSet) }

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

  val StringChars = !"\"\\".contains(_: Char)
  val escape = P("\\" ~ CharIn("\"/\\bfnrt"))
  val strChars = P(CharsWhile(StringChars))

  val stringLiteral: Parser[String] =
    P("\"" ~/ (strChars | escape).rep.! ~ "\"")

  lazy val array_expr = {
    P("[" ~ expr.rep(sep = "," ~/ Pass) ~ "]")
  }

  val expr: Parser[Any] = {
    bool_const |
      set_const |
      int_const |
      float_const |
      var_par_id ~ "[" ~ int_const ~ "]" |
      var_par_id |
      array_expr
  }

  val annotation = {
    P(pred_ann_id ~ ("(" ~ exprInAnn.rep(sep = ",") ~ ")").?)
  }

  val annotations = P("::" ~ annotation).rep

  lazy val array_exprInAnn = {
    P("[" ~ exprInAnn.rep(sep = ",") ~ "]")
  }

  //
  val exprInAnn: Parser[Any] = P(
    array_exprInAnn |
      annotation |
      expr |
      stringLiteral)

  val param_decl = P(par_type ~ ":" ~ var_par_id ~ "=" ~ expr ~ ";")

  val var_decl = {
    P(var_type ~ ":" ~ var_par_id ~ annotations ~ ("=" ~ expr).? ~ ";")
  }

  val data = """predicate all_different_int(array [int] of var int: x);
array [1..2] of int: X_INTRODUCED_26 = [1,-1];
array [1..18] of int: X_INTRODUCED_108 = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1];
var 0..8: X_INTRODUCED_0;
var 0..8: X_INTRODUCED_1;
var 0..8: X_INTRODUCED_2;
var 0..8: X_INTRODUCED_3;
var 0..8: X_INTRODUCED_4;
var 0..8: X_INTRODUCED_5;
var 0..8: X_INTRODUCED_6;
var 0..8: X_INTRODUCED_7;
var 0..8: X_INTRODUCED_8;
var bool: X_INTRODUCED_9:: is_defined_var;
var bool: X_INTRODUCED_10:: is_defined_var;
var bool: X_INTRODUCED_11:: is_defined_var;
var bool: X_INTRODUCED_12:: is_defined_var;
var bool: X_INTRODUCED_13:: is_defined_var;
var bool: X_INTRODUCED_14:: is_defined_var;
var bool: X_INTRODUCED_15:: is_defined_var;
var bool: X_INTRODUCED_16:: is_defined_var;
var bool: X_INTRODUCED_17:: is_defined_var;
var bool: X_INTRODUCED_18:: is_defined_var;
var bool: X_INTRODUCED_19:: is_defined_var;
var bool: X_INTRODUCED_20:: is_defined_var;
var bool: X_INTRODUCED_21:: is_defined_var;
var bool: X_INTRODUCED_22:: is_defined_var;
var bool: X_INTRODUCED_23:: is_defined_var;
var bool: X_INTRODUCED_24:: is_defined_var;
var bool: X_INTRODUCED_25:: is_defined_var;
var 0..8: satisfies:: output_var:: is_defined_var;
var bool: X_INTRODUCED_28 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_30 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_32 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_34 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_36 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_38 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_40 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_42 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_44 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_46 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_48 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_50 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_52 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_54 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_56 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_58 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_60 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_62 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_64 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_66 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_68 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_70 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_72 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_74 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_76 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_78 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_80 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_82 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_84 ::var_is_introduced :: is_defined_var;
var bool: X_INTRODUCED_86 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_88 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_89 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_90 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_91 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_92 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_93 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_94 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_95 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_96 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_97 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_98 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_99 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_100 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_101 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_102 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_103 ::var_is_introduced :: is_defined_var;
var 0..1: X_INTRODUCED_104 ::var_is_introduced :: is_defined_var;
array [1..9] of var 0..8: pos:: output_array([0..8]) = [X_INTRODUCED_0,X_INTRODUCED_1,X_INTRODUCED_2,X_INTRODUCED_3,X_INTRODUCED_4,X_INTRODUCED_5,X_INTRODUCED_6,X_INTRODUCED_7,X_INTRODUCED_8];
array [1..18] of var 0..8: X_INTRODUCED_107 ::var_is_introduced  = [X_INTRODUCED_88,X_INTRODUCED_89,X_INTRODUCED_90,X_INTRODUCED_91,X_INTRODUCED_92,X_INTRODUCED_93,X_INTRODUCED_94,X_INTRODUCED_95,X_INTRODUCED_96,X_INTRODUCED_97,X_INTRODUCED_98,X_INTRODUCED_99,X_INTRODUCED_100,X_INTRODUCED_101,X_INTRODUCED_102,X_INTRODUCED_103,X_INTRODUCED_104,satisfies];
constraint int_lin_eq(X_INTRODUCED_108,X_INTRODUCED_107,0):: defines_var(satisfies);
constraint all_different_int(pos);
constraint int_lin_le(X_INTRODUCED_26,[X_INTRODUCED_0,X_INTRODUCED_1],-1);
constraint bool_xor(X_INTRODUCED_28,X_INTRODUCED_30,X_INTRODUCED_9):: defines_var(X_INTRODUCED_9);
constraint bool_xor(X_INTRODUCED_32,X_INTRODUCED_34,X_INTRODUCED_10):: defines_var(X_INTRODUCED_10);
constraint bool_xor(X_INTRODUCED_36,X_INTRODUCED_38,X_INTRODUCED_11):: defines_var(X_INTRODUCED_11);
constraint bool_xor(X_INTRODUCED_40,X_INTRODUCED_42,X_INTRODUCED_12):: defines_var(X_INTRODUCED_12);
constraint bool_xor(X_INTRODUCED_44,X_INTRODUCED_46,X_INTRODUCED_13):: defines_var(X_INTRODUCED_13);
constraint bool_xor(X_INTRODUCED_48,X_INTRODUCED_50,X_INTRODUCED_14):: defines_var(X_INTRODUCED_14);
constraint bool_xor(X_INTRODUCED_52,X_INTRODUCED_54,X_INTRODUCED_15):: defines_var(X_INTRODUCED_15);
constraint bool_xor(X_INTRODUCED_56,X_INTRODUCED_58,X_INTRODUCED_16):: defines_var(X_INTRODUCED_16);
constraint bool_xor(X_INTRODUCED_60,X_INTRODUCED_62,X_INTRODUCED_17):: defines_var(X_INTRODUCED_17);
constraint bool_xor(X_INTRODUCED_64,X_INTRODUCED_66,X_INTRODUCED_18):: defines_var(X_INTRODUCED_18);
constraint bool_xor(X_INTRODUCED_34,X_INTRODUCED_32,X_INTRODUCED_19):: defines_var(X_INTRODUCED_19);
constraint bool_xor(X_INTRODUCED_68,X_INTRODUCED_70,X_INTRODUCED_20):: defines_var(X_INTRODUCED_20);
constraint bool_xor(X_INTRODUCED_72,X_INTRODUCED_74,X_INTRODUCED_21):: defines_var(X_INTRODUCED_21);
constraint bool_xor(X_INTRODUCED_76,X_INTRODUCED_78,X_INTRODUCED_22):: defines_var(X_INTRODUCED_22);
constraint bool_xor(X_INTRODUCED_80,X_INTRODUCED_82,X_INTRODUCED_23):: defines_var(X_INTRODUCED_23);
constraint bool_xor(X_INTRODUCED_84,X_INTRODUCED_86,X_INTRODUCED_24):: defines_var(X_INTRODUCED_24);
constraint bool_xor(X_INTRODUCED_82,X_INTRODUCED_80,X_INTRODUCED_25):: defines_var(X_INTRODUCED_25);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_2,X_INTRODUCED_0],1,X_INTRODUCED_28):: defines_var(X_INTRODUCED_28);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_0,X_INTRODUCED_2],1,X_INTRODUCED_30):: defines_var(X_INTRODUCED_30);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_4,X_INTRODUCED_0],1,X_INTRODUCED_32):: defines_var(X_INTRODUCED_32);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_0,X_INTRODUCED_4],1,X_INTRODUCED_34):: defines_var(X_INTRODUCED_34);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_7,X_INTRODUCED_0],1,X_INTRODUCED_36):: defines_var(X_INTRODUCED_36);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_0,X_INTRODUCED_7],1,X_INTRODUCED_38):: defines_var(X_INTRODUCED_38);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_4,X_INTRODUCED_1],1,X_INTRODUCED_40):: defines_var(X_INTRODUCED_40);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_1,X_INTRODUCED_4],1,X_INTRODUCED_42):: defines_var(X_INTRODUCED_42);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_8,X_INTRODUCED_1],1,X_INTRODUCED_44):: defines_var(X_INTRODUCED_44);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_1,X_INTRODUCED_8],1,X_INTRODUCED_46):: defines_var(X_INTRODUCED_46);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_3,X_INTRODUCED_2],1,X_INTRODUCED_48):: defines_var(X_INTRODUCED_48);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_2,X_INTRODUCED_3],1,X_INTRODUCED_50):: defines_var(X_INTRODUCED_50);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_4,X_INTRODUCED_2],1,X_INTRODUCED_52):: defines_var(X_INTRODUCED_52);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_2,X_INTRODUCED_4],1,X_INTRODUCED_54):: defines_var(X_INTRODUCED_54);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_0,X_INTRODUCED_3],1,X_INTRODUCED_56):: defines_var(X_INTRODUCED_56);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_3,X_INTRODUCED_0],1,X_INTRODUCED_58):: defines_var(X_INTRODUCED_58);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_4,X_INTRODUCED_3],1,X_INTRODUCED_60):: defines_var(X_INTRODUCED_60);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_3,X_INTRODUCED_4],1,X_INTRODUCED_62):: defines_var(X_INTRODUCED_62);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_5,X_INTRODUCED_4],1,X_INTRODUCED_64):: defines_var(X_INTRODUCED_64);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_4,X_INTRODUCED_5],1,X_INTRODUCED_66):: defines_var(X_INTRODUCED_66);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_0,X_INTRODUCED_5],1,X_INTRODUCED_68):: defines_var(X_INTRODUCED_68);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_5,X_INTRODUCED_0],1,X_INTRODUCED_70):: defines_var(X_INTRODUCED_70);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_8,X_INTRODUCED_5],1,X_INTRODUCED_72):: defines_var(X_INTRODUCED_72);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_5,X_INTRODUCED_8],1,X_INTRODUCED_74):: defines_var(X_INTRODUCED_74);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_2,X_INTRODUCED_6],1,X_INTRODUCED_76):: defines_var(X_INTRODUCED_76);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_6,X_INTRODUCED_2],1,X_INTRODUCED_78):: defines_var(X_INTRODUCED_78);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_7,X_INTRODUCED_6],1,X_INTRODUCED_80):: defines_var(X_INTRODUCED_80);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_6,X_INTRODUCED_7],1,X_INTRODUCED_82):: defines_var(X_INTRODUCED_82);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_8,X_INTRODUCED_7],1,X_INTRODUCED_84):: defines_var(X_INTRODUCED_84);
constraint int_lin_eq_reif(X_INTRODUCED_26,[X_INTRODUCED_7,X_INTRODUCED_8],1,X_INTRODUCED_86):: defines_var(X_INTRODUCED_86);
constraint bool2int(X_INTRODUCED_9,X_INTRODUCED_88):: defines_var(X_INTRODUCED_88);
constraint bool2int(X_INTRODUCED_10,X_INTRODUCED_89):: defines_var(X_INTRODUCED_89);
constraint bool2int(X_INTRODUCED_11,X_INTRODUCED_90):: defines_var(X_INTRODUCED_90);
constraint bool2int(X_INTRODUCED_12,X_INTRODUCED_91):: defines_var(X_INTRODUCED_91);
constraint bool2int(X_INTRODUCED_13,X_INTRODUCED_92):: defines_var(X_INTRODUCED_92);
constraint bool2int(X_INTRODUCED_14,X_INTRODUCED_93):: defines_var(X_INTRODUCED_93);
constraint bool2int(X_INTRODUCED_15,X_INTRODUCED_94):: defines_var(X_INTRODUCED_94);
constraint bool2int(X_INTRODUCED_16,X_INTRODUCED_95):: defines_var(X_INTRODUCED_95);
constraint bool2int(X_INTRODUCED_17,X_INTRODUCED_96):: defines_var(X_INTRODUCED_96);
constraint bool2int(X_INTRODUCED_18,X_INTRODUCED_97):: defines_var(X_INTRODUCED_97);
constraint bool2int(X_INTRODUCED_19,X_INTRODUCED_98):: defines_var(X_INTRODUCED_98);
constraint bool2int(X_INTRODUCED_20,X_INTRODUCED_99):: defines_var(X_INTRODUCED_99);
constraint bool2int(X_INTRODUCED_21,X_INTRODUCED_100):: defines_var(X_INTRODUCED_100);
constraint bool2int(X_INTRODUCED_22,X_INTRODUCED_101):: defines_var(X_INTRODUCED_101);
constraint bool2int(X_INTRODUCED_23,X_INTRODUCED_102):: defines_var(X_INTRODUCED_102);
constraint bool2int(X_INTRODUCED_24,X_INTRODUCED_103):: defines_var(X_INTRODUCED_103);
constraint bool2int(X_INTRODUCED_25,X_INTRODUCED_104):: defines_var(X_INTRODUCED_104);
solve :: int_search(pos,first_fail,indomain,complete) maximize satisfies;"""

  println(FlatZincFastParser.flatzincModel.parseIterator(data.grouped(1024)))
}