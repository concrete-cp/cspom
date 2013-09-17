package cspom.dimacs;

import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.util.Iterator

import scala.io.Source
import scala.util.matching.Regex

import cspom.CSPOM
import cspom.CSPParseException
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable

final class CNFParser(private val problem: CSPOM) {

  private val PARAMETER = new Regex("""^p cnf (\d+) (\d+)$""");
  private val VAR = new Regex("""(-?\d+)""");

  def parse(is: InputStream) {
    val reader = new BufferedReader(new InputStreamReader(is));

    val lines = Source.fromInputStream(is).getLines.filter(
      s => !(s startsWith "c") && !(s.trim.isEmpty))

    val PARAMETER(nbVars, nbClauses) = try lines.next
    catch {
      case e: Exception => throw new CSPParseException("Parameter line not found", e, -1)
    }

    val variables = for (i <- 1 to nbVars.toInt) yield problem.addVariable(CSPOMVariable.bool("V" + i))

    var currentClause: List[Int] = Nil;
    var countClauses = 0
    for (line <- lines; value <- VAR.findAllIn(line).map(s => s.toInt)) {
      if (value == 0) {
        problem.addConstraint(clause(currentClause, variables));
        countClauses += 1;
        currentClause = Nil;
      } else {
        currentClause ::= value;
      }
    }

    assume(countClauses == nbClauses.toInt)
  }

  private def clause(currentClause: List[Int], variables: IndexedSeq[CSPOMVariable]) = {

    val (clause, parameters) = currentClause map { i =>
      (variables(math.abs(i) - 1), i == 0)
    } unzip

    new CSPOMConstraint("or", clause, Map("revsign" -> parameters))

  }
}
