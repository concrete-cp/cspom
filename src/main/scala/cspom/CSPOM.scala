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
import cspom.variable.IntConstant
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMTrue
import cspom.variable.IntVariable
import cspom.variable.FreeVariable
import cspom.variable.BoolExpression
import cspom.variable.CSPOMFalse
import cspom.extension.Table

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
  private val variableMap = collection.mutable.LinkedHashMap[String, CSPOMVariable]()

  /**
   * @return The variables of this problem.
   */
  def variables = variableMap.values;

  val getVariables = JavaConversions.asJavaCollection(variables)

  /**
   * @param variableName
   *            A variable name.
   * @return The variable with the corresponding name.
   */
  def variable(name: String) = variableMap.get(name);

  /**
   * Collection of all constraints of the problem.
   */
  private val _constraints = collection.mutable.Set[CSPOMConstraint]()

  def constraints = _constraints

  val getConstraints = JavaConversions.setAsJavaSet(constraints)

  /**
   * Adds a variable to the problem.
   *
   * @param variable
   *            The variable to add. It must have an unique name.
   * @throws DuplicateVariableException
   *             If a variable with the same name already exists.
   */
  //  def addVariable[T <: CSPOMVariable](variable: T): T = {
  //    val oldVariable = variableMap.put(variable.name, variable)
  //    require(oldVariable.isEmpty, variable.name + ": a variable of the same name already exists");
  //    variable
  //  }

  //  def addExpression(expression: CSPOMExpression): Seq[CSPOMVariable] = expression match {
  //    case v: CSPOMVariable => Seq(addVariable(v))
  //    case s: CSPOMSeq[_] => s.flatMap(addExpression)
  //    case _ => throw new UnsupportedOperationException
  //  }

  def removeVariable(v: CSPOMVariable) {
    val variable = variableMap(v.name)
    require(variable eq v, s"Variable $variable referenced as ${v.name} in the problem is not the same instance as $v")
    require(constraints(v).isEmpty, v + " is still implied by constraints : " + constraints(v))

    variableMap.remove(v.name);
  }

  private val ctrV = collection.mutable.HashMap[CSPOMVariable, Set[CSPOMConstraint]]().withDefault(_ => Set())

  /**
   * Adds a constraint to the problem.
   *
   * @param constraint
   *            The constraint to add.
   */
  private def addConstraint(constraint: CSPOMConstraint) = {

    require(!constraints.contains(constraint),
      "The constraint " + constraint + " already belongs to the problem");

    for (v <- constraint.scope) {
      val variable = variableMap.getOrElseUpdate(v.name, v)
      require(variable eq v, s"$variable (from problem) and $v (from $constraint) do not refer to the same instance")
    }

    _constraints += constraint

    for (v <- constraint.scope) {
      ctrV(v) += constraint
      //      ctrV += v -> (constraint +: ctrV.getOrElseUpdate(v, Nil))
    }
    _neighbors --= constraint.scope

    constraint
  }

  def removeConstraint(c: CSPOMConstraint) {
    //for (v <- c.scope) { v.removeConstraint(c) } 
    _constraints -= c
    for (v <- c.scope) {
      ctrV(v) -= c
    }
    _neighbors --= c.scope
  }

  def constraints(v: CSPOMVariable) = {
    ctrV(v)
//    .getOrElseUpdate(v,
//      _constraints.iterator.filter(_.scope.contains(v)).toList)
  }

  private val _neighbors = collection.mutable.WeakHashMap[CSPOMVariable, Set[CSPOMVariable]]()

  def neighbors(v: CSPOMVariable) = {
    _neighbors.getOrElseUpdate(v, constraints(v).flatMap(_.scope).toSet - v)
  }

  def ctr(c: CSPOMConstraint): CSPOMConstraint = {
    if (constraints(c)) {
      c
    } else {
      addConstraint(c)
    }
  }

  def ctr(v: BoolVariable) = {
    addConstraint(new CSPOMConstraint(CSPOMTrue, 'eq, Seq(v, CSPOMTrue)))
    //    // replace the variable by the CSPOMTrue constant
    //    val Seq(fc: CSPOMConstraint) = constraints(v)
    //    removeConstraint(fc)
    //    removeVariable(v)
    //    val newConstraint = fc.replacedVar(v, CSPOMTrue)
    //    addConstraint(newConstraint)

  }

  //  def ctr(name: Symbol, scope: Seq[CSPOMExpression], params: Map[String, Any] = Map()): CSPOMConstraint = {
  //    ctr(new CSPOMConstraint(name, scope, params))
  //  }

  @annotation.varargs
  def extCtr(rel: Relation, init: Boolean, vars: CSPOMVariable*): CSPOMConstraint =
    ctr(new CSPOMConstraint('extension, vars, Map("init" -> init, "relation" -> rel)))

  def is(name: Symbol, scope: Seq[CSPOMExpression], params: Map[String, Any] = Map()): CSPOMVariable = {
    val result = CSPOM.aux()
    ctr(new CSPOMConstraint(result, name, scope, params))
    result
  }

  def isInt(name: Symbol, scope: Seq[CSPOMExpression], params: Map[String, Any] = Map()): IntVariable = {
    val result = CSPOM.auxInt()
    ctr(new CSPOMConstraint(result, name, scope, params))
    result
  }

  def isBool(name: Symbol, scope: Seq[CSPOMExpression], params: Map[String, Any] = Map()): BoolVariable = {
    val result = CSPOM.auxBool()
    ctr(new CSPOMConstraint(result, name, scope: _*))
    result
  }

  private val constants = new HashMap[Int, IntConstant]()

  def constant(value: Int) = constants.getOrElseUpdate(value, IntConstant(value))

  override def toString = {
    val vars = variables.mkString("\n")

    val cons = constraints.mkString("\n")

    s"$vars\n$cons\n${variables.size} variables and ${constraints.size} constraints"
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

    for (v <- variableMap.values) {
      stb.append("node [\n");
      stb.append("id \"").append(v.name).append("\"\n");
      stb.append("label \"").append(v.name).append("\"\n");
      stb.append("]\n");
    }

    var gen = 0;
    for (c <- constraints) {
      c.scope.toSeq match {
        case Seq(source, target) =>
          stb.append("edge [\n");
          stb.append("source \"").append(source).append("\"\n");
          stb.append("target \"").append(target).append("\"\n");
          stb.append("label \"").append(c.function).append("\"\n");
          stb.append("]\n");
        case s =>
          stb.append("node [\n");
          stb.append("id \"cons").append(gen).append("\"\n");
          stb.append("label \"").append(c.function).append("\"\n");
          stb.append("graphics [\n");
          stb.append("fill \"#FFAA00\"\n");
          stb.append("]\n");
          stb.append("]\n");

          for (v <- s) {
            stb.append("edge [\n");
            stb.append("source \"cons").append(gen).append("\"\n");
            stb.append("target \"").append(v.name).append("\"\n");
            stb.append("]\n");
          }
          gen += 1;
      }
    }
    stb.append("]\n").toString
  }

  def controlInt(solution: Map[String, Int]): Set[CSPOMConstraint] = ???

  //  def control(solution: Map[String, Number]) = {
  //    constraints filterNot { c =>
  //      c.evaluate(c.scope.collect { case v: CSPOMVariable => solution(v.name) })
  //    }
  //  }
  //
  //  def controlInt(solution: Map[String, Int]) = {
  //    constraints flatMap { c =>
  //      val values = c.scope.collect { case v: CSPOMVariable => solution(v.name) }
  //      if (c.evaluate(values)) {
  //        Nil
  //      } else {
  //        List((c, values))
  //      }
  //    }
  //  }
  //
  //  def controlInteger(solution: Map[String, java.lang.Integer]) = {
  //    constraints filterNot { c =>
  //      val tuple = c.scope.collect { case v: CSPOMVariable => solution(v.name).toInt }
  //      c.evaluate(tuple)
  //    }
  //  }

  //  def closeRelations() {
  //    for (c <- constraints) c match {
  //      case c: ExtensionConstraint => c.closeRelation()
  //      case _ =>
  //    }
  //
  //  }
  //  def loadXML(is: InputStream) {
  //    new XCSPParser(this).parse(is)
  //  }
  //
  //  def loadCNF(is: InputStream) {
  //    new CNFParser(this).parse(is)
  //  }
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

    val path = url.getPath();

    if (path endsWith ".gz") {
      new GZIPInputStream(url.openStream());
    } else if (path endsWith ".bz2") {
      val is = url.openStream();
      is.read();
      is.read();
      new CBZip2InputStream(is);
    } else {
      url.openStream();
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
  @throws(classOf[IOException])
  def load(xcspFile: String): CSPOM = {
    val uri = try { new URI(xcspFile) } catch {
      case e: URISyntaxException =>
        throw new IOException("Invalid URI", e);
    }

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
   * @return The loaded CSPOM
   * @throws CSPParseException
   *             If the given file could not be parsed.
   * @throws IOException
   *             If the given file could not be read.
   * @throws DimacsParseException
   */
  @throws(classOf[CSPParseException])
  @throws(classOf[IOException])
  def load(url: URL): CSPOM = {
    val problemIS = problemInputStream(url);

    url.getFile match {
      case name if name.contains(".xml") => XCSPParser.parse(problemIS)
      case name if name.contains(".cnf") => CNFParser.parse(problemIS)
      case _ => throw new IllegalArgumentException("Unhandled file format");
    }

  }

  private val dyn = new DynamicVariable[CSPOM](null)

  def apply[T](f: => T): CSPOM = withResult(f)._1

  def apply[T](f: CSPOM => T): CSPOM = withResult(f)._1

  def withResult[T](f: CSPOM => T): (CSPOM, T) = {
    val p = new CSPOM()
    (p, f(p))
  }

  def withResult[T](f: => T): (CSPOM, T) = withResult { cspom => dyn.withValue(cspom)(f) }

  /**
   * An implicit function that returns the thread-local problem in a model block
   */
  implicit def threadLocalProblem: CSPOM = {
    val s = dyn.value
    require(s ne null, "No implicit problem available; threadLocalProblem can only be used within a problem block")
    s
  }

  def ctr(v: BoolVariable)(implicit problem: CSPOM): CSPOMConstraint = problem.ctr(v)

  def ctr(c: CSPOMConstraint)(implicit problem: CSPOM): CSPOMConstraint = problem.ctr(c)

  def ctr(rel: Relation, init: Boolean)(vars: CSPOMVariable*)(implicit problem: CSPOM) =
    problem.extCtr(rel, init, vars: _*)

  implicit def constant(value: Int)(implicit problem: CSPOM): IntConstant = problem.constant(value)

  implicit def constant(value: Boolean): CSPOMConstant with BoolExpression =
    if (value) CSPOMTrue else CSPOMFalse

  implicit def seq2CSPOMSeq[T <: CSPOMExpression](s: Seq[T]): CSPOMSeq[T] = new CSPOMSeq[T](s)

  implicit def array2CSPOMSeq[T <: CSPOMExpression](s: Array[T]) = new CSPOMSeq[T](s)
  //    var l: List[CSPOMExpression] = Nil
  //    for (i <- s) {
  //      l ::= i
  //    }
  //    new CSPOMSeq(l)
  //  }

  implicit def seq2Rel(s: Seq[Array[Int]]) = new Table(s)

  implicit def aux(): FreeVariable = CSPOMVariable.aux()

  implicit def auxInt(): IntVariable = CSPOMVariable.auxInt()

  implicit def auxBool(): BoolVariable = CSPOMVariable.auxBool()

  @annotation.varargs
  def varOf(values: Int*) = CSPOMVariable.ofInt(values: _*)

  @annotation.varargs
  def varOf(name: String, values: Int*) = CSPOMVariable.ofInt(name = name, values = values: _*)

  def varOfSeq(values: Seq[Int], params: String*) = CSPOMVariable.ofIntSeq(values, params: _*)

  def boolVar() = CSPOMVariable.bool()

  def boolVar(name: String) = CSPOMVariable.bool(name)

  def freeInt(name: String)(implicit problem: CSPOM) = IntVariable.free(name)

  /**
   * Adds a bounded, named variable in the problem.
   *
   * @param name
   *            name of the variable
   * @param lb
   *            lower bound
   * @param ub
   *            upper bound
   * @return The added variable.
   * @throws DuplicateVariableException
   *             if a variable of the same name already exists
   */
  def interVar(name: String, lb: Int, ub: Int) = CSPOMVariable.ofInterval(name, lb, ub)

  /**
   * Adds a bounded, unnamed variable in the problem.
   *
   * @param lb
   *            lower bound
   * @param ub
   *            upper bound
   * @return The added variable.
   */
  def interVar(lb: Int, ub: Int) = CSPOMVariable.ofInterval(lb = lb, ub = ub)
}


