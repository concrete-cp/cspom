package cspom

import java.io.IOException
import java.io.InputStream
import java.net.URI
import java.net.URISyntaxException
import java.net.URL
import java.util.zip.GZIPInputStream
import scala.collection.JavaConversions
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.util.DynamicVariable
import org.apache.tools.bzip2.CBZip2InputStream
import cspom.xcsp.ConstraintParser
import cspom.dimacs.CNFParser
import cspom.extension.Relation
import cspom.variable.CSPOMVariable
import cspom.xcsp.Extension
import cspom.xcsp.XCSPParser
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.IntVariable
import cspom.variable.FreeVariable
import cspom.extension.Table
import cspom.flatzinc.FlatZincParser
import cspom.variable.SimpleExpression
import scala.util.parsing.combinator.JavaTokenParsers
import java.io.StringReader
import scala.util.parsing.input.CharSequenceReader

object NameParser extends JavaTokenParsers {

  def parse: Parser[(String, Seq[Int])] = ident ~ rep("[" ~> wholeNumber <~ "]") ^^ {
    case n ~ i => (n, i.map(_.toInt))
  }
}

/**
 *
 * CSPOM is the central class of the Constraint Satisfaction Problem Object
 * Model. You can create a problem from scratch by instantiating this class and
 * then using the ctr and var methods. The CSPOM.load() method can be used in
 * order to create a CSPOM object from an XCSP file.
 * <p>
 * The CSPOM class adheres to the following definition :
 *
 * A CSP is defined as a pair (X, C), X being a finite set of variables, and C a
 * finite set of constraints.
 *
 * A domain is associated to each variable x in X. Each constraint involves a
 * finite set of variables (its scope) and defines a set of allowed and
 * forbidden instantiations of these variables.
 *
 * @author Julien Vion
 * @see CSPOMConstraint
 * @see CSPOMVariable
 *
 */
class CSPOM {

  /**
   * Map used to easily retrieve a variable according to its name.
   */
  private var _namedExpressions = Map[String, CSPOMExpression[_]]()

  /**
   * @return The named expressions of this problem.
   */
  def namedExpressions = _namedExpressions

  def getExpressions = JavaConversions.asJavaCollection(namedExpressions)

  /**
   * @param variableName
   *            A variable name.
   * @return The variable with the corresponding name.
   */
  def expression(name: String) = {
    NameParser.parse(new CharSequenceReader(name)).map(Some(_)).getOrElse(None).flatMap {
      case (n, s) => getInSeq(namedExpressions.get(n), s)
    }

  }

  private def getInSeq(e: Option[CSPOMExpression[_]], s: Seq[Int]): Option[CSPOMExpression[_]] = {
    if (s.isEmpty) {
      e
    } else {
      e.collect {
        case v: CSPOMSeq[_] => getInSeq(Some(v(s.head)), s.tail)
      } flatten
    }
  }

  /**
   * Collection of all constraints of the problem.
   */
  private val _constraints = collection.mutable.Set[CSPOMConstraint[_]]()

  def constraints = _constraints

  val getConstraints = JavaConversions.setAsJavaSet(constraints)

  def nameExpression[A <: CSPOMExpression[_]](e: A, n: String): A = {
    require(!namedExpressions.contains(n), s"${namedExpressions(n)} is already named $n")
    _namedExpressions += n -> e
    e
  }

  private val ctrV = collection.mutable.HashMap[CSPOMExpression[_], Set[CSPOMConstraint[_]]]()

  /**
   * Adds a constraint to the problem.
   *
   * @param constraint
   *            The constraint to add.
   */
  private def addConstraint[A](constraint: CSPOMConstraint[A]) = {

    require(!constraints.contains(constraint),
      "The constraint " + constraint + " already belongs to the problem");

    _constraints += constraint

    for (
      e <- Iterator(constraint.result) ++ constraint.arguments;
      v <- e.flatten
    ) {
      val cs = ctrV.getOrElse(v, Set()) + constraint
      ctrV(v) = cs
    }

    constraint
  }

  def removeConstraint(c: CSPOMConstraint[_]) {
    require(_constraints(c))
    _constraints -= c

    //require((Iterator(c.result) ++ c.arguments).forall(ctrV(_)(c)))

    for (
      e <- Iterator(c.result) ++ c.arguments;
      v <- e.flatten;
      oc <- ctrV.get(v)
    ) {

      val s = oc - c

      if (s.isEmpty) {
        ctrV -= v
      } else {
        ctrV(v) = s
      }
    }
  }

  def constraints(v: CSPOMExpression[_]) = {
    ctrV.getOrElse(v, Set())
  }

  def ctr[A](c: CSPOMConstraint[A]): CSPOMConstraint[A] = {
    if (constraints(c)) {
      println(s"WARNING : $c already belongs to the problem")
      c
    } else {
      addConstraint(c)
    }
  }

  def ctr(v: BoolVariable) = {
    addConstraint(CSPOMConstraint('eq, v, CSPOMConstant(true)))
  }

  def is(name: Symbol, scope: Seq[CSPOMExpression[_]], params: Map[String, Any] = Map()): FreeVariable = {
    val result = new FreeVariable(Map("var_is_introduced" -> Unit))
    ctr(new CSPOMConstraint(result, name, scope, params))
    result
  }

  def isInt(name: Symbol, scope: Seq[CSPOMExpression[_]], params: Map[String, Any] = Map()): IntVariable = {
    val result = IntVariable.free(Map("var_is_introduced" -> Unit))
    ctr(new CSPOMConstraint(result, name, scope, params))
    result
  }

  def isBool(name: Symbol, scope: Seq[CSPOMExpression[_]], params: Map[String, Any] = Map()): BoolVariable = {
    val result = new BoolVariable(Map("var_is_introduced" -> Unit))
    ctr(CSPOMConstraint(result, name, scope, params))
    result
  }

  override def toString = {
    val vars = namedExpressions.toSeq.sortBy(_._1).map { case (name, variable) => s"$name: $variable" }.mkString("\n")
    val vn = new VariableNames(this)
    val cons = constraints.iterator.map(_.toString(vn)).mkString("\n")

    s"$vars\n$cons\n${namedExpressions.size} named expressions, ${ctrV.size} first-level expressions and ${constraints.size} constraints"
  }

  //  /**
  //   * Generates the constraint network graph in the GML format. N-ary
  //   * constraints are represented as nodes.
  //   *
  //   * @return a String containing the GML representation of the constraint
  //   *         network.
  //   */
  //  def toGML = {
  //    val stb = new StringBuilder();
  //    stb.append("graph [\n");
  //    stb.append("directed 0\n");
  //
  //    for ((k, e) <- namedExpressions) {
  //      stb.append("node [\n");
  //      stb.append("id \"").append(k).append("\"\n");
  //      stb.append("label \"").append(k).append("\"\n");
  //      stb.append("]\n");
  //    }
  //
  //    var gen = 0;
  //    for (c <- constraints) {
  //      c.scope.toSeq match {
  //        case Seq(source, target) =>
  //          stb.append("edge [\n");
  //          stb.append("source \"").append(source).append("\"\n");
  //          stb.append("target \"").append(target).append("\"\n");
  //          stb.append("label \"").append(c.function).append("\"\n");
  //          stb.append("]\n");
  //        case s =>
  //          stb.append("node [\n");
  //          stb.append("id \"cons").append(gen).append("\"\n");
  //          stb.append("label \"").append(c.function).append("\"\n");
  //          stb.append("graphics [\n");
  //          stb.append("fill \"#FFAA00\"\n");
  //          stb.append("]\n");
  //          stb.append("]\n");
  //
  //          for (v <- s) {
  //            stb.append("edge [\n");
  //            stb.append("source \"cons").append(gen).append("\"\n");
  //            stb.append("target \"").append(v.name).append("\"\n");
  //            stb.append("]\n");
  //          }
  //          gen += 1;
  //      }
  //    }
  //    stb.append("]\n").toString
  //  }

  def replaceExpression(which: CSPOMExpression[_], by: CSPOMExpression[_]) = {
    _namedExpressions = _namedExpressions.map {
      case (n, e) => n -> e.replaceVar(which, by)
    }
  }

  def referencedExpressions = ctrV.keySet ++ namedExpressions.values

}

object CSPOM {

  val VERSION = "CSPOM 1.3-SNAPSHOT"

  /**
   * Opens an InputStream according to the given URL. If URL ends with ".gz"
   * or ".bz2", the stream is inflated accordingly.
   *
   * @param url
   *            URL to open
   * @return An InputStream corresponding to the given URL
   * @throws IOException
   *             If the InputStream could not be opened
   */
  @throws(classOf[IOException])
  def problemInputStream(url: URL): InputStream = {

    val path = url.getPath

    val is = url.openStream

    if (path endsWith ".gz") {
      new GZIPInputStream(is)
    } else if (path endsWith ".bz2") {
      is.read()
      is.read()
      new CBZip2InputStream(is)
    } else {
      is
    }

  }

  /**
   * Loads a CSPOM from a given XCSP file.
   *
   * @param xcspFile
   *            Either a filename or an URI. Filenames ending with .gz or .bz2
   *            will be inflated accordingly.
   * @return The loaded CSPOM
   * @throws CSPParseException
   *             If the given file could not be parsed.
   * @throws IOException
   *             If the given file could not be read.
   * @throws DimacsParseException
   */
  @throws(classOf[CSPParseException])
  def load(xcspFile: String): (CSPOM, Map[Symbol, Any]) = {
    val uri = new URI(xcspFile)

    if (uri.isAbsolute) {
      load(uri.toURL)
    } else {
      load(new URL("file://" + uri));
    }

  }

  /**
   * Loads a CSPOM from a given XCSP file.
   *
   * @param url
   *            An URL locating the XCSP file. Filenames ending with .gz or
   *            .bz2 will be inflated accordingly.
   * @return The loaded CSPOM and the list of original variable names
   * @throws CSPParseException
   *             If the given file could not be parsed.
   * @throws IOException
   *             If the given file could not be read.
   * @throws DimacsParseException
   */
  @throws(classOf[CSPParseException])
  @throws(classOf[IOException])
  def load(url: URL): (CSPOM, Map[Symbol, Any]) = {
    val problemIS = problemInputStream(url);

    url.getFile match {
      case name if name.contains(".xml") => XCSPParser.parse(problemIS)
      case name if name.contains(".cnf") => CNFParser.parse(problemIS)
      case name if name.contains(".fzn") => FlatZincParser.parse(problemIS)
      case _ => throw new IllegalArgumentException("Unhandled file format");
    }

  }

  private val dyn = new DynamicVariable[CSPOM](null)

  def apply(f: => Any): CSPOM = {
    apply { p: CSPOM => dyn.withValue(p)(f) }
  }

  def apply(f: CSPOM => Any): CSPOM = {
    val p = new CSPOM()
    f(p)
    p
  }

  /**
   * An implicit function that returns the thread-local problem in a model block
   */
  implicit def threadLocalProblem: CSPOM = {
    val s = dyn.value
    require(s ne null, "No implicit problem available; threadLocalProblem can only be used within a problem block")
    s
  }

  def ctr(v: BoolVariable)(implicit problem: CSPOM): CSPOMConstraint[Boolean] = problem.ctr(v)

  def ctr[A](c: CSPOMConstraint[A])(implicit problem: CSPOM): CSPOMConstraint[A] = problem.ctr(c)

  def table[A](rel: Relation[A], init: Boolean, vars: Seq[SimpleExpression[A]]): CSPOMConstraint[Boolean] =
    CSPOMConstraint('extension, vars, Map("init" -> init, "relation" -> rel))

  implicit def seq2Rel(s: Seq[Seq[Int]]) = new Table(s)

  implicit def constant[A](c: A): CSPOMConstant[A] = CSPOMConstant(c)

}


