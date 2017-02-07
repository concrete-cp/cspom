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
import scala.util.Try
import cspom.CSPOMGoal

final object CNFParser extends CSPOM.Parser {

  private val PARAMETER = new Regex("""^p cnf (\d+) (\d+)$""");
  private val VAR = new Regex("""(-?\d+)""");

  def apply(is: InputStream): Try[CSPOM] = Try {
    val reader = new BufferedReader(new InputStreamReader(is));

    val lines = Source.fromInputStream(is).getLines.filter(
      s => !(s startsWith "c") && !(s.trim.isEmpty))

    val PARAMETER(nbVars, nbClauses) = try lines.next
    catch {
      case e: Exception => throw new CSPParseException("Parameter line not found", e, -1)
    }

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

      CSPOM.goal(CSPOMGoal.Satisfy)
    }

    require(countClauses == nbClauses.toInt)

    problem
  }

  private def clause(currentClause: List[Int], variables: IndexedSeq[CSPOMExpression[Boolean]]) = {
    require(!currentClause.contains(0))

    val (positive, negative) = currentClause.partition(_ > 0)

    /*
     * Variable indices starts at 1 in Dimacs format, 0 in IndexedSeq
     */

    CSPOMConstraint('clause)(positive.map(i => variables(i - 1)), negative.map(i => variables(-i - 1)))

  }
}
