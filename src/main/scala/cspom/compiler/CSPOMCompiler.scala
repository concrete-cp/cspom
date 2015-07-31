package cspom.compiler;

import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashSet
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.Statistic
import cspom.StatisticsManager
import cspom.TimedException
import cspom.VariableNames
import cspom.util.BitVector
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMVariable

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

    //   val allCompilers = BitVector(constraintCompilers.indices)
    //
    //    val toCompile = new HashMap[Int, BitVector]

    for (c <- problem.constraints) {
      constraints.put(c.id, c)
      // toCompile.put(c.id, allCompilers)
    }

    var queue = QueueSet(constraints.keys)

    //val toCompile = Array.fill(constraintCompilers.size)(fullQS)

    while (queue.nonEmpty) {

      val (next, nextQueue) = queue.dequeue
      queue = nextQueue
      //println(s"$next / ${CSPOMConstraint.id} : ${constraints.get(next)}")

      //      val willCompile = toCompile.get(next)
      //      toCompile.remove(next)

      for {
        //        wc <- willCompile
        //        c <- wc.iterator
        //        c <- 
        compiler <- constraintCompilers
        constraint <- constraints.get(next)
      } {
        //println(s"$next, $compiler")
        // println(s"Compiling ${constraint.toString(vn)}")
        //lazy val string = constraint.toString(vn)
        val delta = compile(compiler, constraint)

        for (rc <- delta.removed) {
          assert(!problem.constraintSet(rc), s"$compiler: $rc is still present")
          constraints.remove(rc.id)
          //toCompile.remove(rc.id)
        }

//        for (ac <- delta.added) {
//          assert(problem.constraintSet(ac), s"$compiler: $ac is not present")
//          constraints.put(ac.id, ac)
//        }

        constraints ++= delta.added.map(c => c.id -> c)

        queue = queue.enqueueAll(delta.added.view.map(_.id))

        //val addToCompile = if (compiler.selfPropagation) allCompilers else allCompilers - c
        //        val enqueueVar = vars(delta.added)
        //        for (
        //          v <- enqueueVar
        //        ) {
        //          val constraints = problem.deepConstraints(v).view.map(_.id).toSet
        //          queue = queue.enqueueAll(constraints)
        //
        //          //          for (cons <- constraints) {
        //          //            toCompile(cons) = addToCompile
        //          //          }
        //        }

        //            //println(s"enqueueing $enqueueCons")
        //            for (i <- queues.indices) {
        //              if (self || i != currentQueue)
        //                queues(i) = queues(i).enqueueAll(enqueueCons)
        //            }

        //          if (delta.nonEmpty) {
        //            //if (delta.nonEmpty && compiler == MergeEq) println(s"$string: $delta")
        //            changed = true
        //            updateQueues(compiler, delta, constraints, toCompile, i)
        //          }
      }

      //toCompile(i).remove(constraint.id)
      //println  

    }

    problem
  }

  private def updateQueues(
    compiler: ConstraintCompiler,
    delta: Delta,
    constraints: HashMap[Int, CSPOMConstraint[_]],
    queues: Array[QueueSet],
    currentQueue: Int): Unit = {
    logger.debug(delta.toString(vn))

    for (rc <- delta.removed) {
      assert(!problem.constraintSet(rc), s"$compiler: $rc is still present")
      constraints.remove(rc.id)
    }

    for (c <- delta.added) {
      assert(problem.constraintSet(c), s"$compiler: $c is not present")
      constraints.put(c.id, c)
    }

    val enqueueVar = vars(delta.added)
    logger.debug(s"${delta.added}: Enqueuing constraints for ${enqueueVar.map(v => v -> problem.deepConstraints(v).map(c => s"${c.id}.$c"))}")
    enqueue(enqueueVar, compiler.selfPropagation, queues, currentQueue)
  }

  private def vars(added: Seq[CSPOMConstraint[_]]): collection.Set[CSPOMVariable[_]] = {
    val enqueueVar = new LinkedHashSet[CSPOMVariable[_]]
    for (c <- added) {
      enqueueVar ++= c.fullScope.flatMap(_.flattenVariables)
    }
    enqueueVar
  }

  private def enqueue(vars: Iterable[CSPOMVariable[_]], self: Boolean, queues: Array[QueueSet], currentQueue: Int) = {
    for (
      v <- vars
    ) {
      val enqueueCons = problem.deepConstraints(v).view.map(_.id).toSet
      //println(s"enqueueing $enqueueCons")
      for (i <- queues.indices) {
        if (self || i != currentQueue)
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

