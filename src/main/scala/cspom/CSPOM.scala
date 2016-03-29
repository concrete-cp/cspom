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
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

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

  private var postponed: List[CSPOMConstraint[_]] = Nil

  var goal: Option[CSPOMGoal] = None

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

  def goal_=(g: CSPOMGoal): Unit = this.goal = Some(g)

  def getContainers(e: CSPOMExpression[_]): collection.Set[(CSPOMSeq[_], Int)] = containers(e)

  def addAnnotation(expressionName: String, annotationName: String, annotation: Any): Unit = {
    annotations(expressionName) += (annotationName -> annotation)
  }

  def getAnnotations(expressionName: String): Annotations = annotations(expressionName)

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

  def constraints: Iterator[CSPOMConstraint[_]] = _constraints.iterator

  def constraintSet: collection.Set[CSPOMConstraint[_]] = _constraints //.toSet

  val getConstraints: java.util.Iterator[CSPOMConstraint[_]] = constraints.asJava

  def nameExpression[A <: CSPOMExpression[_]](e: A, n: String): A = {
    require(!namedExpressions.contains(n), s"${namedExpressions(n)} is already named $n")
    namedExpressions += n -> e
    expressionNames(e) += n
    registerContainer(e)

    @annotation.tailrec
    def resolve(e: A): A = {
      postponed.find(_.flattenedScope.contains(e)) match {
        case Some(c) =>
          resolvePostponed(c)
          resolve(e)

        case _ => e
      }
    }

    resolve(e)

  }

  private def registerContainer(e: CSPOMExpression[_]): Unit = {

    for {
      s <- PartialFunction.condOpt(e) { case s: CSPOMSeq[_] => s }
      (c, i) <- s.withIndex
    } {
      logger.trace(s"Registering $s")
      containers.getOrElseUpdate(c, new LinkedHashSet()) += ((s, i))
      registerContainer(c)
    }

  }

  /**
   * Adds a constraint to the problem.
   *
   * @param constraint
   *            The constraint to add.
   */
  private[cspom] def addConstraint[A](constraint: CSPOMConstraint[A]): CSPOMConstraint[A] = {

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

  def removeConstraint(c: CSPOMConstraint[_]): Unit = {
    require(_constraints(c), s"$c not in problem")
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
    for {
      s <- PartialFunction.condOpt(e) { case s: CSPOMSeq[_] => s }
      (e, i) <- s.withIndex
    } {
      logger.trace(s"Deregistering $s")
      val set = containers(e)
      set -= ((s, i))
      if (!isReferenced(e)) {
        removeContainer(e)
      }
    }
  }

  def isReferenced(e: CSPOMExpression[_]): Boolean =
    ctrV(e).nonEmpty || expressionNames(e).nonEmpty || containers.get(e).exists(_.nonEmpty)

  def constraints(v: CSPOMExpression[_]): SortedSet[CSPOMConstraint[_]] = {
    ctrV(v) //++ containers(v).flatMap { case (container, _) => constraints(container) }
  }

  def deepConstraints(v: CSPOMExpression[_]): Iterable[CSPOMConstraint[_]] = {
    containers.get(v) match {
      case None => ctrV(v)
      case Some(c) =>
        val buf = new ArrayBuffer() ++ ctrV(v)
        for ((container, _) <- c) {
          deepConstraints(container, buf)
        }
        buf
    }
  }

  private def deepConstraints(v: CSPOMExpression[_], c: ArrayBuffer[CSPOMConstraint[_]]): ArrayBuffer[CSPOMConstraint[_]] = {
    c ++= ctrV(v)
    for (
      cont <- containers.get(v);
      (container, _) <- cont
    ) {
      deepConstraints(container, c)
    }
    c
  }

  def replaceExpression[R: TypeTag, T <: R](which: CSPOMExpression[R], by: CSPOMExpression[T]): Seq[(CSPOMExpression[R], CSPOMExpression[R])] = {
    logger.debug(s"replacing $which (${namesOf(which)}) with $by (${namesOf(by)})") // from ${Thread.currentThread().getStackTrace.toSeq}")
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

    goal.foreach {
      case CSPOMGoal.Minimize(`which`, p) =>
        goal = CSPOMGoal.Minimize(by.asInstanceOf[CSPOMExpression[Int]], p)
      case CSPOMGoal.Maximize(`which`, p) =>
        goal = CSPOMGoal.Maximize(by.asInstanceOf[CSPOMExpression[Int]], p)
      case _ =>
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
      postpone(c)
      resolvePostponed(c)
      c
    }
  }

  def ctr(v: SimpleExpression[Boolean]): CSPOMConstraint[Boolean] = {
    ctr(CSPOMConstraint('eq)(v, CSPOMConstant(true)))
  }

  def define[R](f: => R)(g: R => CSPOMConstraint[_]): R = {
    val r = f
    postpone(g(r))
    r
  }

  def defineInt(f: SimpleExpression[Int] => CSPOMConstraint[_]): SimpleExpression[Int] = {
    define(IntVariable.free())(f)
  }

  def defineFree(f: SimpleExpression[_] => CSPOMConstraint[_]): SimpleExpression[_] =
    define(new FreeVariable())(f)

  def defineBool(f: SimpleExpression[Boolean] => CSPOMConstraint[_]): SimpleExpression[Boolean] =
    define(new BoolVariable())(f)

  def postpone[A](c: CSPOMConstraint[A]): CSPOMConstraint[A] = {
    postponed ::= c
    c
  }

  private def resolvePostponed(c: CSPOMConstraint[_]): Unit = {
    postponed = resolvePostponed(c.flattenedScope, postponed)
  }

  @annotation.tailrec
  private def resolvePostponed(nodes: Set[CSPOMExpression[_]], postponed: List[CSPOMConstraint[_]]): List[CSPOMConstraint[_]] = {
    val (post, remaining) = postponed.partition { c => c.flattenedScope.exists(nodes) }
    if (post.isEmpty) {
      remaining
    } else {
      post.foreach(addConstraint(_))
      resolvePostponed(post.map(_.flattenedScope).reduce(_ ++ _), remaining)
    }
  }

  private def crawl(c: CSPOMConstraint[_], visited: Set[CSPOMConstraint[_]] = Set.empty): Set[CSPOMConstraint[_]] = {
    if (visited(c)) {
      visited
    } else {
      val inCScope = c.flattenedScope.toSet
      postponed.filter(const => inCScope(const.result)).foldLeft(visited + c) {
        case (visit, c) => visit ++ crawl(c, visit)
      }
    }
  }

  def getPostponed: Seq[CSPOMConstraint[_]] = postponed

  override def toString: String = {
    val vn = new VariableNames(this)
    val vars = referencedExpressions.map(e => (vn.names(e), e)).sortBy(_._1).map {
      case (name, variable) => s"$name: $variable"
    }.mkString("\n")

    val cons = constraints.map(_.toString(vn)).mkString("\n")

    s"$vars\n$cons\n${namedExpressions.size} named expressions, ${ctrV.size} first-level expressions and ${constraints.size} constraints"
  }

}

object CSPOM {

  type Parser = InputStream => Try[CSPOM]

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
  def loadXCSP(file: String): Try[CSPOM] = load(file2url(file), XCSPParser)

  def loadFZ(file: String): Try[CSPOM] = load(file2url(file), FlatZincParser)

  def loadCNF(file: String): Try[CSPOM] = load(file2url(file), CNFParser)

  def file2url(file: String): URL = {
    val uri = new URI(file)
    if (uri.isAbsolute && Option(uri.getScheme).isDefined) {
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
  def load(url: URL, format: Parser): Try[CSPOM] = {
    problemInputStream(url).flatMap(format)
  }

  def load(url: URL): Try[CSPOM] =
    autoParser(url)
      .map(p => load(url, p))
      .getOrElse(Failure(new IllegalArgumentException("Unknown file format")))

  def load(file: String): Try[CSPOM] = load(file2url(file))

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

    def in(rel: Relation[A]): CSPOMConstraint[Boolean] = CSPOMConstraint('extension)(vars: _*) withParam ("init" -> false, "relation" -> rel)
    def notIn(rel: Relation[A]): CSPOMConstraint[Boolean] = CSPOMConstraint('extension)(vars: _*) withParam ("init" -> true, "relation" -> rel)
  }

  implicit def seq2Rel(s: Seq[Seq[Int]]): Relation[Int] = new Table(s)

  implicit def constant[A <: AnyVal: TypeTag](c: A): CSPOMConstant[A] = CSPOMConstant(c)

  implicit def seq2CSPOMSeq[A: TypeTag](c: Seq[CSPOMExpression[A]]): CSPOMSeq[A] = {
    CSPOMSeq(c.toIndexedSeq, 0 until c.size)
  }

  implicit def constantSeq[A <: AnyVal: TypeTag](c: Seq[A]): CSPOMSeq[A] = CSPOMSeq(c.map(constant), 0 until c.size)

  implicit def matrix(sc: StringContext) = Table.MatrixContext(sc)

  def goal(g: CSPOMGoal)(implicit problem: CSPOM): Unit = {
    problem.goal = g
  }

}


