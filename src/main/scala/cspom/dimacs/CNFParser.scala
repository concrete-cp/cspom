package cspom.dimacs;

import cspom.variable.CSPOMVariable
import cspom.{ CSPParseException, CSPOM }
import java.io.{ InputStreamReader, InputStream, BufferedReader }
import java.util.Iterator
import scala.io.Source
import scala.util.matching.Regex

final class CNFParser(private val problem: CSPOM) {

  private val PARAMETER = new Regex("""^p cnf (\d+) (\d+)$""");
  private val VAR = new Regex("""(-?\d+)""");

  def parse(is: InputStream) {
    val reader = new BufferedReader(new InputStreamReader(is));

    var currentClause: List[Int] = Nil;

    val lines = Source.fromInputStream(is).getLines.filter(
      s => !(s startsWith "c") && !(s.trim.isEmpty))

    val PARAMETER(nbVars, nbClauses) = try lines.next
    catch {
      case e => throw new CSPParseException("Parameter line not found", e, -1)
    }

    (1 to nbVars.toInt).foreach(i => problem.addVariable(CSPOMVariable.bool("V" + i)))

    var countClauses = 0
    for (line <- lines; value <- VAR.findAllIn(line).map(s => s.toInt)) {
      if (value == 0) {
        problem.ctr(clause(currentClause));
        countClauses += 1;
        currentClause = Nil;
      } else {
        currentClause ::= value;
      }
    }
    
    assume(countClauses == nbClauses.toInt)
  }

  private def clause(currentClause: List[Int]) = {

    val (clause, parameters) = currentClause map { i =>
      ("V" + math.abs(i), if (i > 0) "0" else "1")
    } unzip

    "or" + parameters.mkString("{", ", ", "}") + clause.mkString("(", ", ", ")")

  }
}
