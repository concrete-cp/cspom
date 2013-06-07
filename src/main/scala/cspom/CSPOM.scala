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
import cspom.compiler.ConstraintParser
import cspom.constraint.CSPOMConstraint
import cspom.constraint.FunctionalConstraint
import cspom.constraint.GeneralConstraint
import cspom.constraint.Predicate
import cspom.dimacs.CNFParser
import cspom.extension.ExtensionConstraint
import cspom.extension.Relation
import cspom.variable.AuxVar
import cspom.variable.CSPOMDomain
import cspom.variable.CSPOMVariable
import cspom.xcsp.Extension
import cspom.xcsp.XCSPParser
import cspom.variable.TrueDomain

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
final class CSPOM {

  /**
   * Map used to easily retrieve a variable according to its name.
   */
  private val variableMap = new LinkedHashMap[String, CSPOMVariable]

  /**
   * @return The variables of this problem.
   */
  val variables = variableMap.values;

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
  private var _constraints: Set[CSPOMConstraint] = Set.empty

  def constraints = _constraints

  val getConstraints = JavaConversions.setAsJavaSet(constraints)

  private var _functionalConstraints: Set[FunctionalConstraint] = Set.empty

  def functionalConstraints = _functionalConstraints

  private var _generalConstraints: Set[GeneralConstraint] = Set.empty

  def generalConstraints = _generalConstraints

  /**
   * Adds a variable to the problem.
   *
   * @param variable
   *            The variable to add. It must have an unique name.
   * @throws DuplicateVariableException
   *             If a variable with the same name already exists.
   */
  def addVariable[T <: CSPOMVariable](variable: T): T = {
    val oldVariable = variableMap.put(variable.name, variable)
    require(oldVariable == None, variable.name + ": a variable of the same name already exists");
    variable
  }

  def removeVariable(v: CSPOMVariable) {
    require(v.constraints.isEmpty, v + " is still implied by constraints : " + v.constraints)

    variableMap.remove(v.name);
  }

  /**
   * Adds a constraint to the problem.
   *
   * @param constraint
   *            The constraint to add.
   */
  def addConstraint(constraint: CSPOMConstraint) = {

    assume(!constraints.contains(constraint),
      "The constraint " + constraint + " already belongs to the problem");

    _constraints += constraint

    constraint match {
      case c: FunctionalConstraint => _functionalConstraints += c
      case c: GeneralConstraint => _generalConstraints += c
      case _ =>
    }

    for (v <- constraint.scope) { v.registerConstraint(constraint) }
    constraint
  }

  def removeConstraint(c: CSPOMConstraint) {
    for (v <- c.scope) { v.removeConstraint(c) }
    _constraints -= c
    c match {
      case c: FunctionalConstraint => _functionalConstraints -= c
      case c: GeneralConstraint => _generalConstraints -= c
      case _ =>
    }
  }

  def ctr(v: AuxVar): CSPOMConstraint = {
    // dereify
    val Seq(fc: FunctionalConstraint) = v.constraints.toSeq
    removeConstraint(fc)
    removeVariable(v)
    val newConstraint = new GeneralConstraint(Predicate(fc.predicate.function, fc.predicate.parameters), fc.arguments)
    addConstraint(newConstraint)
  }

  def parseCtr(ctr: String) = ConstraintParser.split(ctr, this)

  @annotation.varargs
  def ctr(name: String, scope: CSPOMVariable*): CSPOMConstraint = {
    addConstraint(new GeneralConstraint(name, scope: _*))
  }

  @annotation.varargs
  def ctr(name: String, parameters: Any, scope: CSPOMVariable*): CSPOMConstraint = {
    addConstraint(new GeneralConstraint(name, parameters, scope: _*))
  }

  @annotation.varargs
  def ctr(rel: Relation, init: Boolean, vars: CSPOMVariable*): CSPOMConstraint =
    addConstraint(new ExtensionConstraint(rel, init, vars))

  @annotation.varargs
  def is(name: String, scope: CSPOMVariable*): AuxVar = {
    val result = aux()
    addConstraint(new FunctionalConstraint(result, name, scope: _*))
    result
  }

  @annotation.varargs
  def is(name: String, params: Any, scope: CSPOMVariable*): AuxVar = {
    val result = aux()
    addConstraint(new FunctionalConstraint(result, name, params, scope: _*))
    result
  }

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
  def interVar(name: String, lb: Int, ub: Int) =
    addVariable(CSPOMVariable.ofInterval(name, lb, ub))

  /**
   * Adds a bounded, unnamed variable in the problem.
   *
   * @param lb
   *            lower bound
   * @param ub
   *            upper bound
   * @return The added variable.
   */
  def interVar(lb: Int, ub: Int) =
    addVariable(CSPOMVariable.ofInterval(lb = lb, ub = ub))

  def aux(): AuxVar = addVariable(new AuxVar())

  @annotation.varargs
  def varOf[T](values: T*) = addVariable(CSPOMVariable.ofSeq(values = values))

  @annotation.varargs
  def varOf[T](name: String, values: T*) = addVariable(CSPOMVariable.ofSeq(name = name, values = values))

  private val constants = new HashMap[Any, CSPOMVariable]()

  def constant[T](value: T) = constants.getOrElseUpdate(value,
    addVariable(CSPOMVariable.constant(value)))

  def boolVar() = addVariable(CSPOMVariable.bool())

  def boolVar(name: String) = addVariable(CSPOMVariable.bool(name))

  override def toString = {
    val vars = variables.map(v =>
      s"$v = ${v.domainOption.map(_.toString).getOrElse("?")}").mkString("\n")

    val cons = constraints.mkString("\n")

    s"$vars\n$cons"
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
      if (c.arity > 2) {
        stb.append("node [\n");
        stb.append("id \"cons").append(gen).append("\"\n");
        stb.append("label \"").append(c.description).append("\"\n");
        stb.append("graphics [\n");
        stb.append("fill \"#FFAA00\"\n");
        stb.append("]\n");
        stb.append("]\n");

        for (v <- c.scope) {
          stb.append("edge [\n");
          stb.append("source \"cons").append(gen).append("\"\n");
          stb.append("target \"").append(v.name).append("\"\n");
          stb.append("]\n");
        }
        gen += 1;
      } else if (c.arity == 2) {
        stb.append("edge [\n");
        stb.append("source \"").append(c.scope(0)).append("\"\n");
        stb.append("target \"").append(c.scope(1)).append("\"\n");
        stb.append("label \"").append(c.description).append("\"\n");
        stb.append("]\n");
      }
    }
    stb.append("]\n").toString
  }

  def toXCSP = {
    var domains: Map[CSPOMDomain[_], String] = Map.empty
    var did = 0
    for (v <- variables) {
      if (!domains.contains(v.domain)) {
        domains += v.domain -> ("D" + did)
        did += 1
      }
    }

    val relations = new HashMap[(Relation, Boolean), String]()
    val genPredicates = new HashMap[(Predicate, Int), String]()
    val funcPredicates = new HashMap[(Predicate, Int), String]()

    var rid = 0
    var pid = 0

    constraints.foreach {
      case c: ExtensionConstraint if (!relations.contains((c.relation, c.init))) =>
        relations.put((c.relation, c.init), "R" + rid)
        rid += 1

      case c: FunctionalConstraint if (!funcPredicates.contains((c.predicate, c.arity))) =>
        funcPredicates.put((c.predicate, c.arity), "P" + pid)
        pid += 1

      case c: GeneralConstraint if (!genPredicates.contains((c.predicate, c.arity))) =>
        genPredicates.put((c.predicate, c.arity), "P" + pid)
        pid += 1

      case _ =>

    }

    <instance>
      <presentation maxConstraintArity={ constraints.map(_.arity).max.toString }/>
      <domains>
        {
          domains.map {
            case (d, n) =>
              <domain name={ n } nbValues={ d.size.toString }>{ d.toXCSP }</domain>
          }
        }
      </domains>
      <variables nbVariables={ variables.size.toString }>
        {
          variables map { v =>
            <variable name={ v.name } domain={ domains(v.domain) }/>
          }
        }
      </variables>
      <relations nbRelations={ relations.size.toString }>
        {
          relations map {
            case ((r, init), n) =>
              <relation name={ n } arity={ r.arity.toString } nbTuples={ r.size.toString } semantics={ (if (init) "conflicts" else "supports") }>
                { r.tupleString }
              </relation>
          }
        }
      </relations>
      <predicates nbPredicates={ (genPredicates.size + funcPredicates.size).toString }>
        {
          (
            genPredicates map {
              case ((p, a), n) =>
                <predicate name={ n }>
                  <parameters>{ (0 until a) map ("int X" + _) }</parameters>
                  <expression><functional>{ p.function + (0 until a).map("X" + _).mkString("(", ", ", ")") }</functional></expression>
                </predicate>

            }) ++ (funcPredicates map {
              case ((p, a), n) =>
                <predicate name={ n }>
                  <parameters>{ (0 until a) map ("int X" + _) }</parameters>
                  <expression><functional>eq(X0, { p.function + (1 until a).map("X" + _).mkString("(", ", ", ")") })</functional></expression>
                </predicate>

            })
        }
      </predicates>
      <constraints nbConstraints={ constraints.size.toString }>
        {

          constraints.zipWithIndex map {
            case (c, i) =>
              <constraint name={
                "C" + i
              } arity={
                c.arity.toString
              } scope={
                c.scope.map(_.name).mkString(" ")
              } reference={
                c match {
                  case c: ExtensionConstraint => relations(c.relation, c.init)
                  case c: GeneralConstraint => genPredicates(c.predicate, c.arity)
                  case c: FunctionalConstraint => funcPredicates(c.predicate, c.arity)
                }
              }>
                {
                  c match {
                    case c: GeneralConstraint =>
                      <parameters>{ c.scope.map(_.name).mkString(" ") }</parameters>
                    case c: FunctionalConstraint =>
                      <parameters>{ c.scope.map(_.name).mkString(" ") }</parameters>
                    case _ =>
                  }
                }
              </constraint>

          }
        }
      </constraints>
    </instance>
  }

  def control(solution: Map[String, Number]) = {
    constraints filter { c =>
      !c.evaluate(c.scope map (v =>
        solution.getOrElse(v.name, v.domain.values.head)))
    }
  }

  def controlInt(solution: Map[String, Int]) = {
    constraints flatMap { c =>
      val values = c.scope map (v =>
        solution.getOrElse(v.name, v.domain.values.head))
      if (c.evaluate(values)) {
        Nil
      } else {
        List((c, values))
      }
    }
  }

  def controlInteger(solution: Map[String, java.lang.Integer]) = {
    constraints filter { c =>
      val tuple = c.scope map { v =>
        solution.get(v.name) match {
          case Some(value) => value.toInt
          case None => v.domain.values.head
        }
      }
      !c.evaluate(tuple)
    }
  }

  //  def closeRelations() {
  //    for (c <- constraints) c match {
  //      case c: ExtensionConstraint => c.closeRelation()
  //      case _ =>
  //    }
  //
  //  }
  def loadXML(is: InputStream) {
    new XCSPParser(this).parse(is)
  }

  def loadCNF(is: InputStream) {
    new CNFParser(this).parse(is)
  }
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
    val problem = new CSPOM();
    val problemIS = problemInputStream(url);

    url.getFile match {
      case name if name.contains(".xml") => problem.loadXML(problemIS)
      case name if name.contains(".cnf") => problem.loadCNF(problemIS)
      case _ => throw new IllegalArgumentException("Unhandled file format");
    }

    problem;
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
  def interVar(name: String, lb: Int, ub: Int)(implicit problem: CSPOM) = problem.interVar(name, lb, ub)

  /**
   * Adds a bounded, unnamed variable in the problem.
   *
   * @param lb
   *            lower bound
   * @param ub
   *            upper bound
   * @return The added variable.
   */
  def interVar(lb: Int, ub: Int)(implicit problem: CSPOM) = problem.interVar(lb, ub)

  //  def ctr(typ: String)(vars: CSPOMVariable*)(implicit problem: CSPOM) =
  //    problem.ctr(typ, vars: _*)

  def ctr(v: AuxVar)(implicit problem: CSPOM): CSPOMConstraint = problem.ctr(v)

  def ctr(rel: Relation, init: Boolean)(vars: CSPOMVariable*)(implicit problem: CSPOM) =
    problem.ctr(rel, init, vars: _*)
  //
  //  def is(typ: String)(vars: CSPOMVariable*)(implicit problem: CSPOM) = {
  //    problem.is(typ, vars: _*)
  //  }

  implicit class CSPOMSymbConstraint(typ: Symbol) {
    def apply(vars: CSPOMVariable*)(implicit problem: CSPOM): AuxVar = {
      problem.is(typ.name, vars: _*)
    }
    def apply(params: Any)(vars: CSPOMVariable*)(implicit problem: CSPOM): AuxVar = {
      problem.is(typ.name, params, vars: _*)
    }
  }

  implicit def constantVar(value: Int)(implicit problem: CSPOM): CSPOMVariable = problem.constant(value)

  implicit def aux()(implicit problem: CSPOM): AuxVar = new AuxVar()

  def varOf[T](values: T*)(implicit problem: CSPOM) = problem.varOf(values: _*)

  def varOf[T](name: String, values: T*)(implicit problem: CSPOM) = problem.varOf(name, values: _*)

  def boolVar()(implicit problem: CSPOM) = problem.boolVar()

  def boolVar(name: String)(implicit problem: CSPOM) = problem.boolVar(name)
}


