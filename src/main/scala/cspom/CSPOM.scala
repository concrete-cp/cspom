package cspom

import cspom.compiler.ConstraintParser
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import cspom.dimacs.CNFParser
import cspom.variable.CSPOMVariable
import cspom.xcsp.XCSPParser
import java.io.{ IOException, InputStream }
import java.net.{ URL, URI, URISyntaxException }
import java.util.zip.GZIPInputStream
import org.apache.tools.bzip2.CBZip2InputStream
import scala.collection.mutable.{ HashSet, LinkedHashMap }
import scala.collection.JavaConversions
import scala.util.matching.Regex
import cspom.variable.CSPOMDomain
import cspom.extension.Relation
import scala.collection.mutable.LinkedHashSet
import cspom.extension.ExtensionConstraint
import cspom.constraint.FunctionalConstraint
import cspom.constraint.Predicate

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
  val constraints = new LinkedHashSet[CSPOMConstraint]

  val getConstraints = JavaConversions.mutableSetAsJavaSet(constraints)

  /**
   * The constraint compiler used by this CSPOM instance.
   */
  private var constraintParser: ConstraintParser = null

  /**
   * Adds a variable to the problem.
   *
   * @param variable
   *            The variable to add. It must have an unique name.
   * @throws DuplicateVariableException
   *             If a variable with the same name already exists.
   */
  def addVariable[T >: Any](variable: CSPOMVariable): CSPOMVariable = {
    val oldVariable = variableMap.put(variable.name, variable)
    assume(oldVariable == None, variable.name + ": a variable of the same name already exists");
    variable
  }

  def removeVariable(v: CSPOMVariable) {
    assume(v.constraints.isEmpty, v + " is still implied by constraints")

    variableMap.remove(v.name);
  }

  /**
   * Adds a constraint to the problem.
   *
   * @param constraint
   *            The constraint to add.
   */
  def addConstraint(constraint: CSPOMConstraint) = {
    val added = constraints.add(constraint)
    assume(added,
      "The constraint " + constraint + " already belongs to the problem");

    for (v <- constraint.scope) { v.registerConstraint(constraint) }
    constraint
  }

  def removeConstraint(c: CSPOMConstraint): Unit = {
    for (v <- c.scope) { v.removeConstraint(c) }
    constraints.remove(c)
  }

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

  def varOf[T](values: T*) = addVariable(CSPOMVariable.ofSeq(values = values))

  def varOf[T](name: String, values: T*) = addVariable(CSPOMVariable.ofSeq(name = name, values = values))

  def boolVar() = addVariable(CSPOMVariable.bool())

  def boolVar(name: String) = addVariable(CSPOMVariable.bool(name))

  /**
   * Adds functional constraints to the problem. The given predicate will be
   * parsed and the appropriate constraints added to the problem. The
   * predicate may be complex and is usually translated to a set of
   * constraints and auxiliary variables. Use variable names in the predicate
   * to reference them. These variables must already be added to the problem.
   *
   * @param string
   *            A predicate.
   */
  def ctr(string: String) {
    if (constraintParser == null) {
      constraintParser = new ConstraintParser(this);
    }
    constraintParser.split(string);
  }

  def le(v0: CSPOMVariable, v1: CSPOMVariable) =
    addConstraint(new GeneralConstraint("le", v0, v1));

  override def toString = {
    variables.map(v =>
      v + " = " + (if (v.domain == null) '?' else v.domain)).mkString(" ; ") +
      "\n" + constraints.mkString("\n")
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

    var relations: Map[(Relation, Boolean), String] = Map.empty

    var predicates: Map[(Predicate, Int), String] = Map.empty

    var rid = 0
    var pid = 0

    constraints.foreach {
      case c: ExtensionConstraint if (!relations.contains((c.relation, c.init))) =>
        relations += (c.relation, c.init) -> ("R" + rid)
        rid += 1

      case c: FunctionalConstraint if (!predicates.contains((c.predicate, c.arity))) =>
        predicates += (c.predicate, c.arity) -> ("P" + pid)
        pid += 1

      case c: GeneralConstraint if (!predicates.contains((c.predicate, c.arity))) =>
        predicates += (c.predicate, c.arity) -> ("P" + pid)
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
      <predicates nbPredicates={ predicates.size.toString }>
        {
          predicates map {
            case ((p, a), n) =>
              <predicate name={ n }>
                <parameters>{ (0 until a) map ("int X" + _) }</parameters>
                <expression><functional>{ p.function + (0 until a).map("X" + _).mkString("(", ", ", ")") }</functional></expression>
              </predicate>

          }
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
                  case c: GeneralConstraint => predicates(c.predicate, c.arity)
                  case c: FunctionalConstraint => predicates(c.predicate, c.arity)
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
      !c.evaluate(c.scope map { v => solution(v.name) })
    }
  }

  def controlInt(solution: Map[String, Int]) = {
    constraints filter { c =>
      !c.evaluate(c.scope map { v => solution(v.name) })
    }
  }

  def controlInteger(solution: Map[String, java.lang.Integer]) = {
    constraints filter { c =>
      val tuple = c.scope map { v =>
        solution(v.name).toInt
      }
      !c.evaluate(tuple)
    }
  }
}

object CSPOM {

  val VERSION = """Rev:\ (\d+)""".r.findFirstMatchIn("$Rev$").get.group(1).toInt

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

    if (uri.isAbsolute) load(uri.toURL)
    else load(new URL("file://" + uri));

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

    if (url.getFile() contains ".xml") {
      new XCSPParser(problem).parse(problemIS);
    } else if (url.getFile() contains ".cnf") {
      new CNFParser(problem).parse(problemIS);
    } else {
      throw new IllegalArgumentException("Unhandled file format");
    }
    problem;
  }

}
