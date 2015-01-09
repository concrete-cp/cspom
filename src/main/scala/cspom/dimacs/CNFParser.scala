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
import CSPOM._
import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression

final object CNFParser {

  private val PARAMETER = new Regex("""^p cnf (\d+) (\d+)$""");
  private val VAR = new Regex("""(-?\d+)""");

  def parse(is: InputStream): (CSPOM, Map[Symbol, Any]) = {
    val reader = new BufferedReader(new InputStreamReader(is));

    val lines = Source.fromInputStream(is).getLines.filter(
      s => !(s startsWith "c") && !(s.trim.isEmpty))

    val PARAMETER(nbVars, nbClauses) = try lines.next
    catch {
      case e: Exception => throw new CSPParseException("Parameter line not found", e, -1)
    }

    var names: Seq[String] = null
    var countClauses = 0
    val problem = CSPOM { implicit problem =>

      val (variables, ns) = (for (i <- 1 to nbVars.toInt) yield {
        new BoolVariable() withName ("V" + i)
      }).unzip

      var currentClause: List[Int] = Nil;

      for (line <- lines; value <- VAR.findAllIn(line).map(s => s.toInt)) {
        if (value == 0) {
          ctr(clause(currentClause, variables));
          countClauses += 1;
          currentClause = Nil;
        } else {
          currentClause ::= value;
        }
      }

      names = ns
    }

    require(countClauses == nbClauses.toInt)

    (problem, Map('variables -> names))
  }

  private def clause(currentClause: List[Int], variables: IndexedSeq[CSPOMExpression[Boolean]]) = {

    val (clause, parameters) = currentClause
      .map { i => (variables(math.abs(i) - 1), i == 0)
      }
      .unzip

    CSPOMConstraint('or, clause, Map("revsign" -> parameters))

  }
}
