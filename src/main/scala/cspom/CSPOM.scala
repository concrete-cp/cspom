package cspom

import java.io.IOException
import java.io.InputStream
import java.net.URI
import java.net.URL
import java.util.zip.GZIPInputStream
import scala.annotation.migration
import scala.collection.JavaConversions
import scala.collection.SortedSet
import scala.collection.mutable.LinkedHashSet
import scala.language.implicitConversions
import scala.reflect.macros.blackbox.Context
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader
import org.apache.tools.bzip2.CBZip2InputStream
import com.typesafe.scalalogging.LazyLogging
import cspom.dimacs.CNFParser
import cspom.extension.Relation
import cspom.extension.Table
import cspom.flatzinc.FlatZincParser
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.FreeVariable
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression
import cspom.xcsp.XCSPParser
import scala.reflect.runtime.universe._
import scala.collection.JavaConverters._
import scala.util.Failure

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
class CSPOM extends LazyLogging {

  implicit private val ConstraintOrdering = Ordering.by { c: CSPOMConstraint[_] => c.id }

  /**
   * Map used to easily retrieve a variable according to its name.
   */
  private val namedExpressions = collection.mutable.HashMap[String, CSPOMExpression[_]]()

  private val expressionNames = collection.mutable.HashMap[CSPOMExpression[_], SortedSet[String]]().withDefaultValue(SortedSet.empty)

  private[cspom] val containers = collection.mutable.HashMap[CSPOMExpression[_], LinkedHashSet[(CSPOMSeq[_], Int)]]()

  private val ctrV = collection.mutable.LinkedHashMap[CSPOMExpression[_], SortedSet[CSPOMConstraint[_]]]().withDefaultValue(SortedSet.empty)

  private val annotations = collection.mutable.HashMap[String, Annotations]().withDefaultValue(Annotations())

  private def getContainers[R](e: CSPOMExpression[R]): Option[collection.Set[(CSPOMSeq[R], Int)]] =
    containers.get(e).map(_.asInstanceOf[collection.Set[(CSPOMSeq[R], Int)]])

  /**
   * Collection of all constraints of the problem.
   */
  private val _constraints = collection.mutable.LinkedHashSet[CSPOMConstraint[_]]()

  def getExpressions: java.util.Map[String, CSPOMExpression[_]] = namedExpressions.asJava

  private val postponed = collection.mutable.LinkedHashSet[CSPOMConstraint[_]]()

  /**
   * @param variableName
   *            A variable name.
   * @return The variable with the corresponding name.
   */
  def expression(name: String): Option[CSPOMExpression[_]] = {
    NameParser.parse(new CharSequenceReader(name)).map(Some(_)).getOrElse(None).flatMap {
      case (n, s) => getInSeq(namedExpressions.get(n), s)
    }

  }

  def getContainers(e: CSPOMExpression[_]) = containers(e)

  def addAnnotation(expressionName: String, annotationName: String, annotation: Any): Unit = {
    annotations(expressionName) += (annotationName -> annotation)
  }

  def getAnnotations(expressionName: String) = annotations(expressionName)

  private def getInSeq(e: Option[CSPOMExpression[_]], s: Seq[Int]): Option[CSPOMExpression[_]] = s match {
    case Seq() => e
    case head +: tail =>
      e
        .collect {
          case v: CSPOMSeq[_] => getInSeq(Some(v(head)), tail)
        }
        .flatten
  }

  def namesOf(e: CSPOMExpression[_]): Iterable[String] = {
    val direct = expressionNames(e)
    val inContainers = for {
      cl <- containers.get(e).toIterable
      (seq, index) <- cl
      s <- namesOf(seq)
    } yield {
      s"$s[$index]"
    }

    direct ++ inContainers
  }

  def variable(name: String): Option[CSPOMVariable[_]] = {
    expression(name).collect {
      case v: CSPOMVariable[_] => v
    }
  }

  def constraints = _constraints.iterator

  def constraintSet = _constraints //.toSet

  val getConstraints: java.util.Iterator[CSPOMConstraint[_]] = constraints.asJava

  def nameExpression[A <: CSPOMExpression[_]](e: A, n: String): A = {
    require(!namedExpressions.contains(n), s"${namedExpressions(n)} is already named $n")
    namedExpressions += n -> e
    expressionNames(e) += n
    registerContainer(e)
    e
  }

  private def registerContainer(e: CSPOMExpression[_]): Unit = {
    e match {
      case s: CSPOMSeq[_] =>
        for ((c, i) <- s.withIndex) {
          containers.getOrElseUpdate(c, new LinkedHashSet()) += ((s, i))
          registerContainer(c)
        }
      case _ =>
    }
  }

  /**
   * Adds a constraint to the problem.
   *
   * @param constraint
   *            The constraint to add.
   */
  private def addConstraint[A](constraint: CSPOMConstraint[A]) = {

    require(!_constraints(constraint),
      "The constraint " + constraint + " already belongs to the problem");

    _constraints += constraint

    for (
      v <- constraint.fullScope
    ) {
      ctrV(v) += constraint
      registerContainer(v)
    }

    constraint
  }

  def removeConstraint(c: CSPOMConstraint[_]) {
    require(_constraints(c), s"$c does not involve $this (not in ${_constraints})")
    _constraints -= c

    //require((Iterator(c.result) ++ c.arguments).forall(ctrV(_)(c)))

    for (
      v <- c.fullScope
    ) {

      ctrV(v) -= c
      if (ctrV(v).isEmpty) {
        ctrV -= v
        //       freeContainer(v)
      }

      if (!isReferenced(v)) {
        removeContainer(v)
      }

    }
  }

  def removeContainer(e: CSPOMExpression[_]): Unit = {
    //assert(!isReferenced(e))
    e match {
      case s: CSPOMSeq[_] =>
        for ((e, i) <- s.withIndex) {
          val set = containers(e)
          set -= ((s, i))
          if (!isReferenced(e)) {
            removeContainer(e)
          }
        }
      case _ =>
    }
  }

  def isReferenced(e: CSPOMExpression[_]): Boolean =
    ctrV(e).nonEmpty || expressionNames(e).nonEmpty || containers.get(e).exists(_.nonEmpty)

  def constraints(v: CSPOMExpression[_]): SortedSet[CSPOMConstraint[_]] = {
    ctrV(v) //++ containers(v).flatMap { case (container, _) => constraints(container) }
  }

  def deepConstraints(v: CSPOMExpression[_]): SortedSet[CSPOMConstraint[_]] = {
    ctrV(v) ++ containers.getOrElse(v, Set.empty).flatMap { case (container, _) => deepConstraints(container) }
  }

  def replaceExpression[R: TypeTag, T <: R](which: CSPOMExpression[R], by: CSPOMExpression[T]): Seq[(CSPOMExpression[R], CSPOMExpression[R])] = {
    //logger.warn(s"replacing $which (${namesOf(which)}) with $by (${namesOf(by)})") // from ${Thread.currentThread().getStackTrace.toSeq}")
    require(which != by, s"Replacing $which with $by")
    //require((namesOf(which).toSet & namesOf(by).toSet).isEmpty)
    var replaced = List[(CSPOMExpression[R], CSPOMExpression[R])]()

    for (n <- expressionNames(which)) {
      namedExpressions(n) = by
      expressionNames(by) += n
    }
    expressionNames.remove(which)
    for {
      get <- getContainers(which)
      (container, index) <- get
    } {
      val nc = container.replaceIndex(index, by)
      replaced ++:= replaceExpression(container, nc)

      removeContainer(container)
      registerContainer(nc)
    }

    (which, by) :: replaced

  }

  def referencedExpressions: Seq[CSPOMExpression[_]] = {
    val involvedInConstraints = for (e <- ctrV.keysIterator; f <- e.flatten) yield f

    (involvedInConstraints ++ expressionNames.keys).toSeq.distinct
  }

  def expressionsWithNames: Seq[(String, CSPOMExpression[_])] = {
    namedExpressions.toSeq
  }

  def ctr[A](c: CSPOMConstraint[A]): CSPOMConstraint[A] = {
    if (_constraints(c)) {
      logger.warn(s"$c already belongs to the problem")
      c
    } else {
      resolvePostponed(c)
      //addConstraint(c)
      c
    }
  }

  def ctr(v: SimpleExpression[Boolean]): CSPOMConstraint[Boolean] = {
    val c = CSPOMConstraint('eq, Seq(v, CSPOMConstant(true)))
    resolvePostponed(c)
    c
    //addConstraint(c)
  }

  def postpone[A](c: CSPOMConstraint[A]) = {
    postponed += c
    c
  }

  def is(name: scala.Symbol, scope: Seq[CSPOMExpression[_]], params: Map[String, Any] = Map()): FreeVariable = {
    val result = new FreeVariable()
    postpone(CSPOMConstraint(result, name, scope, params))
    result
  }

  def isInt(name: scala.Symbol, scope: Seq[CSPOMExpression[_]], params: Map[String, Any] = Map()): IntVariable = {
    val result = IntVariable.free()
    postpone(CSPOMConstraint(result, name, scope, params))
    result
  }

  def isBool(name: scala.Symbol, scope: Seq[CSPOMExpression[_]], params: Map[String, Any] = Map()): BoolVariable = {
    val result = new BoolVariable()
    postpone(CSPOMConstraint(result, name, scope, params))
    result
  }

  def resolvePostponed(c: CSPOMConstraint[_]): Unit = {
    for (p <- crawl(c)) {
      addConstraint(p)
      postponed -= p
    }
  }

  private def crawl(c: CSPOMConstraint[_], visited: Set[CSPOMConstraint[_]] = Set.empty): Set[CSPOMConstraint[_]] = {
    if (visited(c)) {
      visited
    } else {
      postponed.filter(const => const.fullScope.exists(c.fullScope.contains(_))).foldLeft(visited + c) {
        case (visit, c) => visit ++ crawl(c, visit)
      }
    }
  }

  def getPostponed = postponed.toSet

  override def toString = {
    val vn = new VariableNames(this)
    val vars = referencedExpressions.map(e => (vn.names(e), e)).sortBy(_._1).map {
      case (name, variable) => s"$name: $variable"
    }.mkString("\n")

    val cons = constraints.map(_.toString(vn)).mkString("\n")

    s"$vars\n$cons\n${namedExpressions.size} named expressions, ${ctrV.size} first-level expressions and ${constraints.size} constraints"
  }

  /**
   * Generates the constraint network graph in the GML format. N-ary
   * constraints are represented as nodes.
   *
   * @return a String containing the GML representation of the constraint
   *         network.
   */
  def toGML = {
    val stb = new StringBuilder();
    stb.append("graph [\n");
    stb.append("directed 0\n");

    val vn = new VariableNames(this)

    val variables = referencedExpressions
      .collect {
        case e: CSPOMVariable[_] => e -> vn.names(e)
      }
      .toMap

    for (k <- variables.values.toSeq.sorted) {
      stb.append(s"""
          node [
            id "$k"
            label "$k"
          ]
          """)

    }

    var gen = 0;

    constraints.flatMap { c =>
      c.fullScope.flatMap(_.flatten).collect {
        case v: CSPOMVariable[_] => variables(v)
      } match {
        case Seq(source, target) => s"""
          edge [
            source "$source"
            target "$target"
            label "${c.function.name}"
          ]
          """
        case s =>
          gen += 1
          s"""
          node [
            id "cons$gen"
            label "${c.function.name}"
            graphics [ fill "#FFAA00" ]
          ]
          """ ++ s.flatMap(v => s"""
          edge [
            source "cons$gen"
            target "$v"
          ]
          """)
      }
    }.addString(stb)

    stb.append("]\n").toString
  }

}

object CSPOM {

  type Parser = InputStream => Try[(CSPOM, Map[scala.Symbol, Any])]

  val VERSION = "CSPOM 2.4"

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
  def problemInputStream(url: URL): Try[InputStream] = Try {
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
   */
  def loadXCSP(file: String): Try[(CSPOM, Map[scala.Symbol, Any])] = load(file2url(file), XCSPParser)

  def loadFZ(file: String) = load(file2url(file), FlatZincParser)

  def loadCNF(file: String) = load(file2url(file), CNFParser)

  def file2url(file: String): URL = {
    val uri = new URI(file)
    if (uri.isAbsolute && uri.getScheme != null) {
      uri.toURL
    } else {
      new URL("file:" + uri)
    }
  }

  /**
   * Loads a CSPOM from a given XCSP file.
   *
   * @param url
   *            An URL locating the XCSP file. Filenames ending with .gz or
   *            .bz2 will be inflated accordingly.
   * @return The loaded CSPOM and the list of original variable names
   */
  def load(url: URL, format: Parser): Try[(CSPOM, Map[scala.Symbol, Any])] = {
    problemInputStream(url).flatMap(format)
  }

  def load(url: URL): Try[(CSPOM, Map[scala.Symbol, Any])] =
    autoParser(url)
      .map(p => load(url, p))
      .getOrElse(Failure(new IllegalArgumentException("Unknown file format")))

  def load(file: String): Try[(CSPOM, Map[scala.Symbol, Any])] = load(file2url(file))

  def autoParser(url: URL): Option[Parser] = url.getFile match {
    case name if name.contains(".xml") => Some(XCSPParser)
    case name if name.contains(".cnf") => Some(CNFParser)
    case name if name.contains(".fzn") => Some(FlatZincParser)
    case _                             => None
  }

  def apply(f: CSPOM => Any): CSPOM = {
    val p = new CSPOM()
    f(p)
    p
  }

  def ctr(v: SimpleExpression[Boolean])(implicit problem: CSPOM): CSPOMConstraint[Boolean] = problem.ctr(v)

  def ctr[A](c: CSPOMConstraint[A])(implicit problem: CSPOM): CSPOMConstraint[A] = problem.ctr(c)

  implicit class SeqOperations[A](vars: Seq[SimpleExpression[A]]) {
    def in(rel: Seq[Seq[A]]): CSPOMConstraint[Boolean] = in(new Table(rel))
    def notIn(rel: Seq[Seq[A]]): CSPOMConstraint[Boolean] = notIn(new Table(rel))

    def in(rel: Relation[A]): CSPOMConstraint[Boolean] = CSPOMConstraint('extension, vars, Map("init" -> false, "relation" -> rel))
    def notIn(rel: Relation[A]) = CSPOMConstraint('extension, vars, Map("init" -> true, "relation" -> rel))
  }

  implicit def seq2Rel(s: Seq[Seq[Int]]) = new Table(s)

  implicit def constant[A <: AnyVal: TypeTag](c: A): CSPOMConstant[A] = CSPOMConstant(c)

  implicit def seq2CSPOMSeq[A: TypeTag](c: Seq[CSPOMExpression[A]]): CSPOMSeq[A] = CSPOMSeq(c: _*)

  implicit def constantSeq[A <: AnyVal: TypeTag](c: Seq[A]): CSPOMSeq[A] = CSPOMSeq(c.map(constant): _*)

  import language.experimental.macros

  import scala.reflect.macros.blackbox.Context
  import scala.util.Try

  implicit class MatrixContext(sc: StringContext) {
    def matrix(): Array[Array[Int]] = macro matrixImpl
  }

  def matrixImpl(c: Context)(): c.Expr[Array[Array[Int]]] = {
    import c.universe.{ Try => _, _ }

    val matrix = Try {
      c.prefix.tree match {
        case Apply(_, List(Apply(_, List(Literal(Constant(raw: String)))))) =>

          def toArrayAST(c: List[TermTree]) =
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


