package cspom

import java.io.InputStream
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import java.io.IOException
import java.net.{ URL, URI, URISyntaxException }
import java.util.zip.GZIPInputStream
import org.apache.tools.bzip2.CBZip2InputStream
import scala.collection.JavaConversions
import scala.collection.mutable.{ LinkedHashMap, LinkedHashSet }
import scala.util.matching.Regex
import cspom.variable.CSPOMVariable
import cspom.compiler.ConstraintParser
import cspom.xcsp.XCSPParser
import cspom.dimacs.CNFParser

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

    assume(variableMap.put(variable.name, variable) == None, variable.name
      + ": a variable of the same name already exists");
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
    assume(constraints.add(constraint),
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

  def control(solution: collection.Map[String, Number]) = {
    constraints filter { c =>
      !c.evaluate(c.scope map { v => solution.get(v.name) })
    }
  }

  def controlInt(solution: collection.Map[String, Int]) = {
    constraints filter { c =>
      !c.evaluate(c.scope map { v => solution.get(v.name) })
    }
  }
  
  def controlInteger(solution: collection.Map[String, java.lang.Integer]) = {
    constraints filter { c =>
      !c.evaluate(c.scope map { v => solution.get(v.name) })
    }
  }
}

object CSPOM {
  val VERSION = {
    val ExVers = new Regex("""Rev:\ (\d+)""")
    val ExVers(v) = "$Rev: 603 $"
    v.toInt
  }

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
