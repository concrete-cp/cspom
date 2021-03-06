package cspom

import java.io.{IOException, InputStream}
import java.net.{URI, URL}

import com.typesafe.scalalogging.LazyLogging
import cspom.dimacs.CNFParser
import cspom.extension.{MDDRelation, Relation}
import cspom.flatzinc.FlatZincFastParser
import cspom.variable._
import cspom.xcsp.XCSP3Parser
import org.apache.commons.compress.compressors.{CompressorException, CompressorStreamFactory}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag
import scala.util.{Failure, Try}


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

  val expressionMap = new ExpressionMap
  // private val ConstraintOrdering = Ordering.by { c: CSPOMConstraint[_] => c.id }
  private val ctrV = new mutable.LinkedHashMap[CSPOMExpression[_], mutable.LinkedHashSet[CSPOMConstraint[_]]]()

  //      .withDefaultValue(SortedSet.empty(ConstraintOrdering))
  //  private def getContainers(e: CSPOMExpression[_]): Option[Seq[(CSPOMSeq[_], Int)]] =
  //    Option(containers.get(e)).map(_.asScala.toSeq)
  private val annotations = mutable.HashMap[String, Annotations]().withDefaultValue(Annotations())
  /**
    * Collection of all constraints of the problem.
    */
  private val _constraints = mutable.LinkedHashMap[String, mutable.LinkedHashSet[CSPOMConstraint[_]]]()
  private var postponed: List[CSPOMConstraint[_]] = Nil
  private var _goal: Option[WithParam[CSPOMGoal[_]]] = None


  def setGoal(g: WithParam[CSPOMGoal[_]]): Unit = {
    // println(s"setGoal $g ${Thread.currentThread.getStackTrace.mkString(", ")}")
    resolvePostponed(g.obj.expr)
    this._goal = Some(g)
  }

  def goal: Option[WithParam[CSPOMGoal[_]]] = _goal

  def setGoal(g: CSPOMGoal[_], params: Map[String, Any] = Map()): Unit = setGoal(WithParam(g, params))


  def addAnnotation(expressionName: String, annotationName: String, annotation: Any): Unit = {
    annotations(expressionName) += (annotationName -> annotation)
  }

  def getAnnotations(expressionName: String): Annotations = annotations(expressionName)


  //def constraintSet: collection.Set[CSPOMConstraint[_]] = _constraints //.toSet

  def nameExpression[A <: CSPOMExpression[_]](e: A, n: String): A = {
    expressionMap.nameExpression(e, n)

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
    require(getConstraints(c.function)(c), s"$c not in problem")
    _constraints(c.function) -= c

    //require((Iterator(c.result) ++ c.arguments).forall(ctrV(_)(c)))

    for (v <- c.fullScope; ref <- ctrV.get(v)) {
      // Note: ctrV.get(v) may be empty if v appears several times in the scope
      ref -= c
      if (ref.isEmpty) {
        ctrV -= v
        if (!expressionMap.isReferenced(v)) {
          expressionMap.removeContainer(v)
        }
      }
    }
  }

  def getConstraints(function: String): collection.Set[CSPOMConstraint[_]] = _constraints.getOrElse(function, Set())

  def isReferenced(e: CSPOMExpression[_]): Boolean = ctrV.get(e).exists(_.nonEmpty) || expressionMap.isReferenced(e)

  def constraints(v: CSPOMExpression[_]): Seq[CSPOMConstraint[_]] = {
    /**
      * Should be kept immutable. ConstraintCompiler.replace will concurrently iterate and change
      * contents.
      */
    ctrV.get(v).toSeq.flatten
  }

  def unsafeConstraints(v: CSPOMExpression[_]): Iterator[CSPOMConstraint[_]] = {
    ctrV.get(v).iterator.flatten
  }

  def deepConstraints(v: CSPOMExpression[_]): Iterable[CSPOMConstraint[_]] = {
    val buf = new ArrayBuffer[CSPOMConstraint[_]]()

    def crawl(v: CSPOMExpression[_]): Unit = {
      ctrV.get(v).foreach(buf ++= _)
      for (
        (container, _) <- expressionMap.getContainers(v)
      ) {
        crawl(container)
      }
    }

    crawl(v)
    buf
  }

  def replaceExpression[R: TypeTag, T <: R](which: CSPOMExpression[R], by: CSPOMExpression[T]): Seq[(CSPOMExpression[_], CSPOMExpression[_])] = {
    val replacements = expressionMap.replaceExpression(which, by)

    for ((which, by) <- replacements; g <- goal) {
      g match {
        case WithParam(CSPOMGoal.Minimize(`which`), p) =>
          setGoal(CSPOMGoal.Minimize(by.asInstanceOf[CSPOMExpression[Int]]), p)
        case WithParam(CSPOMGoal.Maximize(`which`), p) =>
          setGoal(CSPOMGoal.Maximize(by.asInstanceOf[CSPOMExpression[Int]]), p)
        case _ =>
      }
    }

    replacements

  }

  def ctr[A](c: CSPOMConstraint[A]): CSPOMConstraint[A] = {
    ctrNetwork(c)
    c
  }

  def ctrNetwork(c: CSPOMConstraint[_]): Seq[CSPOMConstraint[_]] = {
    if (getConstraints(c.function)(c)) {
      logger.warn(s"$c already belongs to the problem")
      Nil
    } else {
      postpone(c)
      resolvePostponed(c)
    }
  }

  def ctr(v: SimpleExpression[Boolean]): CSPOMConstraint[Boolean] = {
    ctr(CSPOMConstraint("eq")(v, CSPOMConstant(true)))
  }

  def ctr(function: String)(arguments: CSPOMExpression[_]*): CSPOMConstraint[Boolean] =
    ctr(CSPOMConstraint(function)(arguments: _*))

  def ctr[A](result: CSPOMExpression[A])(function: String)(arguments: CSPOMExpression[_]*): CSPOMConstraint[A] =
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

  override def toString: String = {
    val vars = referencedExpressions.map(e => (expressionMap.displayName(e), e)).sortBy(_._1).map {
      case (name, variable) => s"$name: $variable"
    }.mkString("\n")

    val cons = constraints.map(_.toString(expressionMap.displayName)).mkString("\n")

    s"$vars\n$cons\n${expressionMap.count} named expressions, ${ctrV.size} first-level expressions and ${constraints.size} constraints"
  }

  def constraints: Iterator[CSPOMConstraint[_]] = _constraints.valuesIterator.flatten

  def referencedExpressions: Seq[CSPOMExpression[_]] = {
    val involvedInConstraints = for (e <- ctrV.keysIterator; f <- e.flatten) yield f

    (involvedInConstraints ++ expressionMap.expressions).toSeq.distinct
  }

  def displayName(e: CSPOMExpression[_]): String = expressionMap.displayName(e)

  def namesOf(e: CSPOMExpression[_]): Iterable[String] = expressionMap.namesOf(e)

  def expression(name: String): Option[CSPOMExpression[_]] = expressionMap.expression(name)

  def variable(name: String): Option[CSPOMVariable[_]] = expressionMap.variable(name)

  def hasConstraint(c: CSPOMConstraint[_]): Boolean = _constraints(c.function)(c)

  /**
    * Adds a constraint to the problem.
    *
    * @param constraint
    * The constraint to add.
    */
  private[cspom] def addConstraint[A](constraint: CSPOMConstraint[A]): CSPOMConstraint[A] = {

    require(!getConstraints(constraint.function)(constraint),
      "The constraint " + constraint + " already belongs to the problem")

    _constraints.getOrElseUpdate(constraint.function, new mutable.LinkedHashSet()) += constraint

    for (
      v <- constraint.fullScope
    ) {
      ctrV.getOrElseUpdate(v, new mutable.LinkedHashSet()) += constraint
      expressionMap.registerContainer(v)
    }

    constraint
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
      post.foreach(c => addConstraint(c))
      resolvePostponed(post.map(_.flattenedScope).reduce(_ ++ _), remaining, post ::: posted)
    }
  }

}

object CSPOM extends LazyLogging {

  type Parser = InputStream => Try[CSPOM]

  /**
    * Loads a CSPOM from a given XCSP3 file.
    *
    * @param file
    * Either a filename or an URI. Filenames ending with .gz or .bz2
    * will be inflated accordingly.
    * @return The loaded CSPOM
    */
  def loadXCSP3(file: String): Try[CSPOM] = load(file2url(file), XCSP3Parser)

  def file2url(file: String): URL = {
    val uri = new URI(file.replace(" ", "%20"))
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
      case e: CompressorException if e.getMessage == "No Compressor found for the stream signature." =>
        logger.info(e.getMessage)
        is
    }
  }

  def loadFZ(file: String): Try[CSPOM] = load(file2url(file), FlatZincFastParser)

  def loadCNF(file: String): Try[CSPOM] = load(file2url(file), CNFParser)

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
    def in(rel: Relation[A]): CSPOMConstraint[Boolean] =
      CSPOMConstraint("extension")(vars: _*) withParam("init" -> false, "relation" -> rel)

    def notIn(rel: Relation[A]): CSPOMConstraint[Boolean] = CSPOMConstraint("extension")(vars: _*) withParam("init" -> true, "relation" -> rel)
  }

  implicit class IntSeqOperations(vars: Seq[SimpleExpression[Int]]) {
    def in(rel: MDDRelation): CSPOMConstraint[Boolean] = SeqOperations(vars).in(rel)

    def notIn(rel: MDDRelation): CSPOMConstraint[Boolean] = SeqOperations(vars).notIn(rel)
  }

  //implicit def seq2Rel(s: Seq[Seq[Int]]): Relation[Int] = new Table(s)

  implicit def constant[A: TypeTag](c: A): CSPOMConstant[A] = CSPOMConstant(c)

  implicit def seq2CSPOMSeq[A: TypeTag](c: Iterable[CSPOMExpression[A]]): CSPOMSeq[A] = {
    val seq = c.toIndexedSeq
    CSPOMSeq(seq, seq.indices)
  }

  implicit def constantSeq[A <: AnyVal : TypeTag](c: Seq[A]): CSPOMSeq[A] = CSPOMSeq(c.map(constant): _*)

  implicit def matrix(sc: StringContext): Relation.MatrixContext = Relation.MatrixContext(sc)

  def goal(g: WithParam[CSPOMGoal[_]])(implicit problem: CSPOM): Unit = {
    problem.setGoal(g)
  }

  def goal(g: CSPOMGoal[_])(implicit problem: CSPOM): Unit = {
    problem.setGoal(g)
  }


}


