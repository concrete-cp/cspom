package cspom.compiler.patterns;

import scala.collection.JavaConversions
import cspom.CSPOM
import cspom.compiler.ConstraintSelector
import cspom.constraint.CSPOMConstraint
import cspom.constraint.GeneralConstraint
import cspom.variable.CSPOMVariable
import scala.util.Random
import scala.util.control.Breaks

final class AllDiff(val problem: CSPOM) extends ConstraintCompiler {

  val neighbors =
    problem.variables map (v =>
      v -> (v.constraints.iterator.filter(DIFF_CONSTRAINT).foldLeft(Set[CSPOMVariable]())(
        (acc, c) => acc ++ c.scope) - v)) toMap

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    constraint.isInstanceOf[GeneralConstraint] &&
      Set("ne", "gt", "lt", "allDifferent").contains(constraint.description)

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    constraint.isInstanceOf[GeneralConstraint] &&
      "ne" == constraint.description ||
      "allDifferent" == constraint.description

  val ITER = 750;

  //val cliqueDetector = new CliqueDetector;

  /**
   * If constraint is part of a larger clique of inequalities, replace it by a
   * larger all-diff constraint.
   *
   * @param constraint
   */
  def alldiff(constraint: CSPOMConstraint) {
    if (!DIFF_CONSTRAINT(constraint)) {
      return ;
    }
    // System.out.print(constraint);

    //val clique: Set[CSPOMVariable] = Set.empty ++ constraint.scope

    // val pool = populate(constraint.scope);
    //print(constraint + " : ")
    val clique = expand(constraint.scope.toSet);
    //println(clique.size)
    if (clique.size > constraint.scope.size) {
      problem.removeConstraint(constraint)
      newAllDiff(clique);
    }

  }

  /**
   * The pool contains all variables that can expand the base clique
   */
  private def populate(base: Set[CSPOMVariable]): Set[CSPOMVariable] =
    base.iterator.map(neighbors(_)).reduceLeft((acc, vs) => acc & vs)

  private def expand(base: Set[CSPOMVariable]) = {

    var largest = base
    var clique = base
    var pool = populate(base)

    //final Set<CSPOMVariable> base = new HashSet<CSPOMVariable>(clique);

    var tabu: Map[CSPOMVariable, Int] = Map.empty
    val mybreaks = new Breaks
    import mybreaks.{ break, breakable }

    breakable {
      for (i <- (1 to ITER)) {
        val (newVar, newTabu) = AllDiff.pick(pool, tabu, i);
        tabu = newTabu
        newVar match {
          case None => {
            /* Could not expand the clique, removing a variable (not from the base) */
            AllDiff.pick((clique -- base).iterator) match {
              case None => break
              case Some(variable) => clique -= variable
            }
            pool = populate(clique)
          }
          case Some(variable) => {
            clique += variable
            if (clique.size > largest.size) {
              largest = clique
            }
            pool -= variable;
            pool &= neighbors(variable)

          }
        }
        //println(System.currentTimeMillis + " : " + clique.size)
      }
    }

    largest
  }

  /**
   * Adds a new all-diff constraint of the specified scope. Any newly subsumed
   * neq/all-diff constraints are removed.
   *
   * @param scope
   */
  private def newAllDiff(scope: Set[CSPOMVariable]) {
    val allDiff = new GeneralConstraint(
      description = "allDifferent",
      scope = scope.toSeq);

    if (problem.constraints.contains(allDiff)) {
      return ;
    }
    problem.addConstraint(allDiff);

    //    var removed = 0
    //    /* Remove newly subsumed neq/alldiff constraints. */
    //    for (
    //      v <- scope; c <- v.constraints if (c != allDiff && ALLDIFF_CONSTRAINT(c) && CliqueDetector.subsumes(allDiff, c))
    //    ) {
    //      removed += 1
    //      problem.removeConstraint(c);
    //    }
    //    println(System.currentTimeMillis + " : removed " + removed + " constraints, " + problem.constraints.size + " left")
  }

  /**
   * If the given constraint is an all-different or neq constraint, remove it
   * if it is subsumed by another difference constraint.
   *
   * @param constraint
   */
  def dropSubsumedDiff(constraint: CSPOMConstraint) = {
    if (ALLDIFF_CONSTRAINT(constraint) &&
      CliqueDetector.haveSubsumingConstraint(constraint, DIFF_CONSTRAINT)) {

      problem.removeConstraint(constraint);
      //println("subsumed ! " + problem.constraints.size + " remaining")
      true;
    } else {
      false;
    }
  }

  override def compile(constraint: CSPOMConstraint) {
    if (!dropSubsumedDiff(constraint)) {
      alldiff(constraint);
    }

  }

}

object AllDiff {
  val RAND = new Random(0);

  val TABU_SIZE = 15;

  private def pick(pool: Set[CSPOMVariable], tabu: Map[CSPOMVariable, Int], iteration: Int): (Option[CSPOMVariable], Map[CSPOMVariable, Int]) = {

    pick(pool.iterator.filter(v =>
      tabu.get(v) match {
        case None => true
        case Some(i) => i < iteration
      })) match {
      case None =>
        (None, tabu)
      case Some(v) =>
        (Some(v), tabu + (v -> (iteration + TABU_SIZE)))
    }

  }

  private def pick[T](it: Iterator[T]): Option[T] = {
    var tie = 1
    var returned: Option[T] = None
    for (i <- it) {
      if (RAND.nextDouble() * tie < 1) returned = Some(i)
      tie += 1
    }
    returned
  }
}
