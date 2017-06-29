package cspom

import java.io.{IOException, InputStream}
import java.net.{URI, URL}
import java.util.{IdentityHashMap, LinkedHashSet}

import com.typesafe.scalalogging.LazyLogging
import cspom.dimacs.CNFParser
import cspom.extension.{MDDRelation, Relation}
import cspom.flatzinc.FlatZincFastParser
import cspom.variable._
import cspom.xcsp.XCSP3Parser
import org.apache.commons.compress.compressors.{CompressorException, CompressorStreamFactory}

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedSet, mutable}
import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.{Failure, Try}

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

  private val ConstraintOrdering = Ordering.by { c: CSPOMConstraint[_] => c.id }
  /**
    * Map used to easily retrieve a variable according to its name.
    */
  private val namedExpressions = collection.mutable.HashMap[String, CSPOMExpression[_]]()
  private val expressionNames = collection.mutable.HashMap[CSPOMExpression[_], SortedSet[String]]().withDefaultValue(SortedSet.empty)
  private[cspom] val containers = new IdentityHashMap[CSPOMExpression[_], LinkedHashSet[(CSPOMSeq[_], Int)]]()
  private val ctrV =
    collection.mutable.LinkedHashMap[CSPOMExpression[_], SortedSet[CSPOMConstraint[_]]]()
      .withDefaultValue(SortedSet.empty(ConstraintOrdering))

  //  private def getContainers(e: CSPOMExpression[_]): Option[Seq[(CSPOMSeq[_], Int)]] =
  //    Option(containers.get(e)).map(_.asScala.toSeq)
  private val annotations = collection.mutable.HashMap[String, Annotations]().withDefaultValue(Annotations())
  /**
    * Collection of all constraints of the problem.
    */
  private val _constraints = collection.mutable.LinkedHashSet[CSPOMConstraint[_]]()
  private val generatedNames = collection.mutable.Map[CSPOMExpression[_], String]()
  private var postponed: List[CSPOMConstraint[_]] = Nil
  private var _goal: Option[WithParam[CSPOMGoal[_]]] = None
  private var generatedId = 0

  def getExpressions: java.util.Map[String, CSPOMExpression[_]] = namedExpressions.asJava

  /**
    * @param name
    * A variable name.
    * @return The variable with the corresponding name.
    */
  def expression(name: String): Option[CSPOMExpression[_]] = {
    NameParser.parse(new CharSequenceReader(name)).map(Some(_)).getOrElse(None).flatMap {
      case (n, s) => getInSeq(namedExpressions.get(n), s)
    }
      .orElse(namedExpressions.get(name))

  }

  def setGoal(g: WithParam[CSPOMGoal[_]]): Unit = {
    resolvePostponed(g.obj.expr)
    this._goal = Some(g)
  }

  def goal: Option[WithParam[CSPOMGoal[_]]] = _goal

  def setGoal(g: CSPOMGoal[_], params: Map[String, Any] = Map()): Unit = setGoal(WithParam(g, params))

  def getContainers(e: CSPOMExpression[_]): Option[Seq[(CSPOMSeq[Any], Seq[Int])]] =
    Option(containers.get(e)).map { l =>
      val m = new mutable.LinkedHashMap[CSPOMSeq[Any], ArrayBuffer[Int]]()
      for (elem <- l.asScala) {
        val key = elem._1
        val bldr = m.getOrElseUpdate(key, new ArrayBuffer[Int]())
        bldr += elem._2
      }

      m.toSeq
    }

  def addAnnotation(expressionName: String, annotationName: String, annotation: Any): Unit = {
    annotations(expressionName) += (annotationName -> annotation)
  }

  def getAnnotations(expressionName: String): Annotations = annotations(expressionName)

  def nextGeneratedName(e: CSPOMExpression[_]): String = e match {
    case CSPOMConstant(v) => v.toString
    //case CSPOMSeq(v) => v.map(displayName).mkString("CSPOMSeq(", ", ", ")")
    case _ =>
      generatedId += 1
      "_" + generatedId
  }

  def namesOf(e: CSPOMExpression[_]): Iterable[String] = {
    val direct = expressionNames(e)
    val inContainers = for {
      cl <- Option(containers.get(e)).toIterable
      (seq, index) <- cl.asScala
      s <- namesOf(seq)
    } yield {
      s"$s[$index]"
    }

    direct ++ inContainers
  }

  def displayName(e: CSPOMExpression[_]): String = e match {
    case CSPOMConstant(c) => c.toString
    case expression =>
      namesOf(expression).toSeq match {
        case Seq() => generatedNames.getOrElseUpdate(expression, nextGeneratedName(expression))
        case cspomNames => cspomNames.sorted.mkString("||")
      }
  }

  def variable(name: String): Option[CSPOMVariable[_]] = {
    expression(name).collect {
      case v: CSPOMVariable[_] => v
    }
  }

  def constraints: Iterator[CSPOMConstraint[_]] = _constraints.iterator

  def constraintSet: collection.Set[CSPOMConstraint[_]] = _constraints //.toSet

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
      val set = containers.get(e)
      set.remove((s, i))
      if (!isReferenced(e)) {
        removeContainer(e)
      }
    }
  }

  def isReferenced(e: CSPOMExpression[_]): Boolean =
    ctrV(e).nonEmpty || expressionNames(e).nonEmpty || Option(containers.get(e)).exists(s => !s.isEmpty)

  def constraints(v: CSPOMExpression[_]): SortedSet[CSPOMConstraint[_]] = {
    ctrV(v) //++ containers(v).flatMap { case (container, _) => constraints(container) }
  }

  def deepConstraints(v: CSPOMExpression[_]): Iterable[CSPOMConstraint[_]] = {
    containers.get(v) match {
      case null => ctrV(v)
      case c =>
        val buf = new ArrayBuffer() ++ ctrV(v)

        for ((container, _) <- c.iterator.asScala) {
          deepConstraints(container, buf)
        }
        buf
    }
  }

  def replaceExpression[R: TypeTag, T <: R](which: CSPOMExpression[R], by: CSPOMExpression[T]): Seq[(CSPOMExpression[_], CSPOMExpression[_])] = {
    lazy val namesOfWhich = which.toString(displayName) // { x => ??? }
    require(which != by, s"Replacing $which with $by")
    //require((namesOf(which).toSet & namesOf(by).toSet).isEmpty)
    var replaced = List[(CSPOMExpression[_], CSPOMExpression[_])]()

    for (n <- expressionNames(which)) {
      namedExpressions(n) = by
      expressionNames(by) += n
    }
    expressionNames.remove(which)

    logger.debug(s"Replacing $which with $by: contained in ${getContainers(which)}")


    for {
      get <- getContainers(which)
      (container, indices) <- get // toList is required to obtain a copy and prevent ConcurrentModificationException
    } {
      val nc = indices.foldLeft(container)((acc, index) => acc.replaceIndex(index, by))
      replaced ++:= replaceExpression(container, nc)

      removeContainer(container)
      registerContainer(nc)
    }

    goal.foreach {
      case WithParam(CSPOMGoal.Minimize(`which`), p) =>
        setGoal(CSPOMGoal.Minimize(by.asInstanceOf[CSPOMExpression[Int]]), p)
      case WithParam(CSPOMGoal.Maximize(`which`), p) =>
        setGoal(CSPOMGoal.Maximize(by.asInstanceOf[CSPOMExpression[Int]]), p)
      case _ =>
    }

    logger.debug(s"replacing $namesOfWhich with ${by.toString(displayName)}") // from ${Thread.currentThread().getStackTrace.toSeq}")

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
    ctrNetwork(c)
    c
  }

  def ctrNetwork(c: CSPOMConstraint[_]): Seq[CSPOMConstraint[_]] = {
    if (_constraints(c)) {
      logger.warn(s"$c already belongs to the problem")
      Nil
    } else {
      postpone(c)
      resolvePostponed(c)
    }
  }

  def ctr(v: SimpleExpression[Boolean]): CSPOMConstraint[Boolean] = {
    ctr(CSPOMConstraint('eq)(v, CSPOMConstant(true)))
  }

  def ctr(function: scala.Symbol)(arguments: CSPOMExpression[_]*): CSPOMConstraint[Boolean] =
    ctr(CSPOMConstraint(function)(arguments: _*))

  def ctr[A](result: CSPOMExpression[A])(function: scala.Symbol)(arguments: CSPOMExpression[_]*): CSPOMConstraint[A] =
    ctr(CSPOMConstraint(result)(function)(arguments: _*))

  def defineInt(f: SimpleExpression[Int] => CSPOMConstraint[_]): SimpleExpression[Int] = {
    define(IntVariable.free())(f)
  }

  def defineFree(f: SimpleExpression[_] => CSPOMConstraint[_]): SimpleExpression[_] =
    define(new FreeVariable())(f)

  def defineBool(f: SimpleExpression[Boolean] => CSPOMConstraint[_]): SimpleExpression[Boolean] =
    define(new BoolVariable())(f)

  def define[R](f: => R)(g: R => CSPOMConstraint[_]): R = {
    val r = f
    postpone(g(r))
    r
  }

  def postpone[A](c: CSPOMConstraint[A]): CSPOMConstraint[A] = {
    postponed ::= c
    c
  }

  def getPostponed: Seq[CSPOMConstraint[_]] = postponed

  override def toString(): String = {
    val vars = referencedExpressions.map(e => (displayName(e), e)).sortBy(_._1).map {
      case (name, variable) => s"$name: $variable"
    }.mkString("\n")

    val cons = constraints.map(_.toString(displayName)).mkString("\n")

    s"$vars\n$cons\n${namedExpressions.size} named expressions, ${ctrV.size} first-level expressions and ${constraints.size} constraints"
  }

  private def getInSeq(e: Option[CSPOMExpression[_]], s: Seq[Int]): Option[CSPOMExpression[_]] = s match {
    case Seq() => e
    case head +: tail =>
      e
        .collect {
          case v: CSPOMSeq[_] => getInSeq(Some(v(head)), tail)
        }
        .flatten
  }

  private def registerContainer(e: CSPOMExpression[_]): Unit = {

    for {
      s <- PartialFunction.condOpt(e) { case s: CSPOMSeq[_] => s }
      (c, i) <- s.withIndex
    } {
      logger.trace(s"Registering $s")
      val set = Option(containers.get(c)).getOrElse {
        val set = new LinkedHashSet[(CSPOMSeq[_], Int)]()
        containers.put(c, set)
        set
      }
      set.add((s, i))
      registerContainer(c)
    }

  }

  /**
    * Adds a constraint to the problem.
    *
    * @param constraint
    * The constraint to add.
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

  private def deepConstraints(v: CSPOMExpression[_], c: ArrayBuffer[CSPOMConstraint[_]]): ArrayBuffer[CSPOMConstraint[_]] = {
    c ++= ctrV(v)
    for (
      cont <- Option(containers.get(v));
      (container, _) <- cont.iterator.asScala
    ) {
      deepConstraints(container, c)
    }
    c
  }

  private def resolvePostponed(c: CSPOMConstraint[_]): Seq[CSPOMConstraint[_]] = {
    resolvePostponed(c.flattenedScope)
  }

  private def resolvePostponed(e: Iterable[CSPOMExpression[_]]): Seq[CSPOMConstraint[_]] = {
    val (posted, remaining) = resolvePostponed(e.toSet, postponed, Nil)
    postponed = remaining
    posted
  }

  @annotation.tailrec
  private def resolvePostponed(nodes: Set[CSPOMExpression[_]], postponed: List[CSPOMConstraint[_]], posted: List[CSPOMConstraint[_]]): (List[CSPOMConstraint[_]], List[CSPOMConstraint[_]]) = {
    val (post, remaining) = postponed.partition { c => c.flattenedScope.exists(nodes) }
    if (post.isEmpty) {
      (posted, remaining)
    } else {
      post.foreach(addConstraint(_))
      resolvePostponed(post.map(_.flattenedScope).reduce(_ ++ _), remaining, post ::: posted)
    }
  }

  private def crawl(c: CSPOMConstraint[_], visited: Set[CSPOMConstraint[_]]): Set[CSPOMConstraint[_]] = {
    if (visited(c)) {
      visited
    } else {
      val inCScope = c.flattenedScope.toSet
      postponed.filter(const => inCScope(const.result)).foldLeft(visited + c) {
        case (visit, c) => visit ++ crawl(c, visit)
      }
    }
  }

}

object CSPOM extends LazyLogging {

  type Parser = InputStream => Try[CSPOM]

  /**
    * Loads a CSPOM from a given XCSP file.
    *
    * @param file
    * Either a filename or an URI. Filenames ending with .gz or .bz2
    * will be inflated accordingly.
    * @return The loaded CSPOM
    */
  def loadXCSP(file: String): Try[CSPOM] = load(file2url(file), XCSP3Parser)

  def loadFZ(file: String): Try[CSPOM] = load(file2url(file), FlatZincFastParser)

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
    * An URL locating the XCSP file. Filenames ending with .gz or
    * .bz2 will be inflated accordingly.
    * @return The loaded CSPOM and the list of original variable names
    */
  def load(url: URL, format: Parser): Try[CSPOM] = {
    problemInputStream(url).flatMap { pis =>
      val cspom = format(pis)
      pis.close()
      cspom
    }
  }

  /**
    * Opens an InputStream according to the given URL. If URL ends with ".gz"
    * or ".bz2", the stream is inflated accordingly.
    *
    * @param url
    * URL to open
    * @return An InputStream corresponding to the given URL
    * @throws IOException
    * If the InputStream could not be opened
    */
  @throws(classOf[IOException])
  def problemInputStream(url: URL): Try[InputStream] = Try {
    val is = url.openStream

    try {
      new CompressorStreamFactory().createCompressorInputStream(is)
    } catch {
      case e: CompressorException if (e.getMessage == "No Compressor found for the stream signature.") =>
        logger.info(e.getMessage)
        is
    }
  }

  def load(file: String): Try[CSPOM] = load(file2url(file))

  def load(url: URL): Try[CSPOM] =
    autoParser(url)
      .map(p => load(url, p))
      .getOrElse(Failure(new IllegalArgumentException(s"${url.getFile}: unknown file format")))

  def autoParser(url: URL): Option[Parser] = url.getFile match {
    case name if name.contains(".xml") => Some(XCSP3Parser)
    case name if name.contains(".cnf") => Some(CNFParser)
    case name if name.contains(".fzn") => Some(FlatZincFastParser)
    case _ => None
  }

  def apply(f: CSPOM => Any): CSPOM = {
    val p = new CSPOM()
    f(p)
    p
  }

  def ctr(v: SimpleExpression[Boolean])(implicit problem: CSPOM): CSPOMConstraint[Boolean] = problem.ctr(v)

  def ctr[A](c: CSPOMConstraint[A])(implicit problem: CSPOM): CSPOMConstraint[A] = problem.ctr(c)

  implicit class SeqOperations[A](vars: Seq[SimpleExpression[A]]) {
    def in(rel: Relation[A]): CSPOMConstraint[Boolean] = CSPOMConstraint('extension)(vars: _*) withParam("init" -> false, "relation" -> rel)

    def notIn(rel: Relation[A]): CSPOMConstraint[Boolean] = CSPOMConstraint('extension)(vars: _*) withParam("init" -> true, "relation" -> rel)
  }

  implicit class IntSeqOperations(vars: Seq[SimpleExpression[Int]]) {
    def in(rel: MDDRelation): CSPOMConstraint[Boolean] = SeqOperations(vars).in(rel)

    def notIn(rel: MDDRelation): CSPOMConstraint[Boolean] = SeqOperations(vars).notIn(rel)
  }

  //implicit def seq2Rel(s: Seq[Seq[Int]]): Relation[Int] = new Table(s)

  implicit def constant[A <: AnyVal : TypeTag](c: A): CSPOMConstant[A] = CSPOMConstant(c)

  implicit def seq2CSPOMSeq[A: TypeTag](c: Seq[CSPOMExpression[A]]): CSPOMSeq[A] = {
    CSPOMSeq(c.toIndexedSeq, 0 until c.size)
  }

  implicit def constantSeq[A <: AnyVal : TypeTag](c: Seq[A]): CSPOMSeq[A] = CSPOMSeq(c.map(constant): _*)

  implicit def matrix(sc: StringContext) = Relation.MatrixContext(sc)

  def goal(g: WithParam[CSPOMGoal[_]])(implicit problem: CSPOM): Unit = {
    problem.setGoal(g)
  }

  def goal(g: CSPOMGoal[_])(implicit problem: CSPOM): Unit = {
    problem.setGoal(g)
  }

}


