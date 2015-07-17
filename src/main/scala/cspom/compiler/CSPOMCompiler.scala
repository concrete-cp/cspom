package cspom.compiler;

import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.WrappedArray
import scala.util.Try
import com.typesafe.scalalogging.LazyLogging
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.Statistic
import cspom.StatisticsManager
import cspom.TimedException
import cspom.VariableNames
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMVariable
import cspom.util.BitVector

/**
 * This class implements some known useful reformulation rules.
 *
 * @author vion
 *
 */
final class CSPOMCompiler(
    private val problem: CSPOM,
    private val constraintCompilers: IndexedSeq[ConstraintCompiler]) extends LazyLogging {

  val vn = new VariableNames(problem)

  private def compile(): CSPOM = {

    val constraints = new HashMap[Int, CSPOMConstraint[_]]

    for (c <- problem.constraints) {
      constraints.put(c.id, c)
    }

    val fullQS = QueueSet(constraints.keys)
    val toCompile = Array.fill(constraintCompilers.size)(fullQS)

    var changed = true

    while (changed) {
      logger.info("Turn")
      changed = false
      for (i <- toCompile.indices) {

        val compiler = constraintCompilers(i)
        logger.info(compiler.toString)

        while (toCompile(i).nonEmpty) {

          val (next, nextQueue) = toCompile(i).dequeue
          toCompile(i) = nextQueue

          for (constraint <- constraints.get(next)) {
            //println(s"Compiling ${constraint.toString(vn)}")
            //lazy val string = constraint.toString(vn)
            val delta = compile(compiler, constraint)

            if (delta.nonEmpty) {
              //if (delta.nonEmpty && compiler == MergeEq) println(s"$string: $delta")
              changed = true
              updateQueues(compiler, delta, constraints, toCompile)
            }

          }

          //toCompile(i).remove(constraint.id)
          //println

        }

      }

    }
    problem
  }

  private def updateQueues(
    compiler: ConstraintCompiler,
    delta: Delta,
    constraints: HashMap[Int, CSPOMConstraint[_]],
    queues: Array[QueueSet]): Unit = {
    logger.info(delta.toString)

    for (rc <- delta.removed) {
      assert(!problem.constraintSet(rc), s"$compiler: $rc is still present")
      constraints.remove(rc.id)
    }

    val enqueueVar = new LinkedHashSet[CSPOMExpression[_]]
    for (c <- delta.added) {
      assert(problem.constraintSet(c), s"$compiler: $c is not present")
      constraints.put(c.id, c)
      enqueueVar ++= c.flattenedScope
    }

    logger.debug(s"Enqueuing constraints for ${enqueueVar.map(v => v -> problem.deepConstraints(v).map(c => s"${c.id}.$c"))}")
    for (
      v <- enqueueVar
    ) {
      val enqueueCons = BitVector(problem.deepConstraints(v).view.map(_.id))
      for (i <- queues.indices) {
        queues(i) = queues(i).enqueueAll(enqueueCons)
      }
    }
  }

  def compile(compiler: ConstraintCompiler, constraint: CSPOMConstraint[_]): Delta = {
    require(problem.constraintSet(constraint), {
      val vn = new VariableNames(problem)
      s"${constraint.toString(vn)} not in $problem"
    })
    CSPOMCompiler.matches += 1

    compiler.mtch(constraint, problem) match {
      case Some(data) =>
        CSPOMCompiler.compiles += 1
        logger.debug(s"$compiler : ${constraint.toString(vn)}")
        compiler.compile(constraint, problem, data)
      case None => Delta()
    }

  }

}

object CSPOMCompiler {
  def compile(problem: CSPOM, compilers: Seq[ConstraintCompiler]): Try[CSPOM] = {
    val pbc = new CSPOMCompiler(problem, compilers.toIndexedSeq)

    val (r, t) = StatisticsManager.time(pbc.compile())

    compileTime += t

    r
  }

  @Statistic
  var matches = 0

  @Statistic
  var compiles = 0

  @Statistic
  var compileTime = 0.0

}

