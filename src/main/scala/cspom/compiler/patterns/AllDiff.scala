package cspom.compiler.patterns;

import scala.collection.JavaConversions
import cspom.CSPOM
import cspom.compiler.ConstraintSelector
import cspom.constraint.CSPOMConstraint
import cspom.constraint.GeneralConstraint
import cspom.variable.CSPOMVariable;
import scala.util.Random

final class AllDiff(val problem: CSPOM) extends ConstraintCompiler {

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    constraint.isInstanceOf[GeneralConstraint] &&
      Set("ne", "gt", "lt", "allDifferent").contains(constraint.description)

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint) =
    constraint.isInstanceOf[GeneralConstraint] &&
      "ne" == constraint.description ||
      "allDifferent" == constraint.description

  val ITER = 750;

  val cliqueDetector = new CliqueDetector;

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

    val pool = populate(constraint.scope);
    val clique = pool
    //		expand(clique, pool);

    if (clique.size > constraint.scope.size) {
      newAllDiff(clique);
    }

  }

  private def populate(base: Iterable[CSPOMVariable]): Set[CSPOMVariable] = {
    if (base.isEmpty) {
      Set.empty
    } else {

      var pool: Set[CSPOMVariable] = Set.empty

      for (c <- base.minBy(_.constraints.size).constraints if DIFF_CONSTRAINT(c))
        pool ++= c.scope

      pool --= base;

      for (v <- base) {
        pool = pool.filter(w => (v eq w) || cliqueDetector.edge(v, w, DIFF_CONSTRAINT))
      }

      pool
    }
  }
  //
  //	private def expand(clique: Set[CSPOMVariable],
  //			pool: Set[CSPOMVariable]) {
  //	  
  //		final Set<CSPOMVariable> largest = new HashSet<CSPOMVariable>(
  //				clique);
  //		final Set<CSPOMVariable> base = new HashSet<CSPOMVariable>(clique);
  //		final Map<CSPOMVariable, Integer> tabu = new HashMap<CSPOMVariable, Integer>();
  //
  //		for (int i = ITER; --i >= 0;) {
  //			final CSPOMVariable newVar = pick(pool, tabu, i);
  //			if (newVar == null) {
  //				if (clique.size() - base.size() <= 0) {
  //					break;
  //				}
  //				int rand = RAND.nextInt(clique.size() - base.size());
  //				CSPOMVariable toRemove = null;
  //				for (CSPOMVariable v : clique) {
  //					if (!base.contains(v) && --rand < 0) {
  //						toRemove = v;
  //						break;
  //					}
  //				}
  //				clique.remove(toRemove);
  //				pool.clear();
  //				populate(pool, clique);
  //			} else {
  //				clique.add(newVar);
  //				if (clique.size() > largest.size()) {
  //					largest.clear();
  //					largest.addAll(clique);
  //				}
  //				pool.remove(newVar);
  //				for (Iterator<CSPOMVariable> itr = pool.iterator(); itr
  //						.hasNext();) {
  //					if (!cliqueDetector.edge(itr.next(), newVar,
  //							DIFF_CONSTRAINT)) {
  //						itr.remove();
  //					}
  //				}
  //			}
  //		}
  //
  //		clique.clear();
  //		clique.addAll(largest);
  //	}

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

    /*
		 * Remove newly subsumed neq/alldiff constraints.
		 */
    for (
      c <- scope.foldLeft(Set[CSPOMConstraint]())((s, v) =>
        s ++ v.constraints.filter(c => c != allDiff && ALLDIFF_CONSTRAINT(c))) if (CliqueDetector.subsumes(allDiff, c))
    ) {
      problem.removeConstraint(c);
    }
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

  private def pick(pool: Set[CSPOMVariable],
    tabu: Map[CSPOMVariable, Int], iteration: Int) = {
    var tie = 1;
    var returned: Option[CSPOMVariable] = None;

    for (
      v <- pool if (tabu.get(v) match {
        case None => true
        case Some(i) => i > iteration
      })
    ) {
      val change = RAND.nextFloat() * tie < 1
      tie += 1
      if (change) returned = Some(v)
    }

    returned match {
      case None =>
        (None, tabu)
      case Some(v) =>
        (Some(v), tabu + (v -> (iteration - TABU_SIZE)))

    }
  }
}
