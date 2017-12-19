package cspom.compiler

;

import com.typesafe.scalalogging.LazyLogging
import cspom.{CSPOM, CSPOMConstraint, Statistic, StatisticsManager}
import cspom.util.VecMap
import org.scalameter.Quantity

import scala.util.Try

/**
  * This class implements some known useful reformulation rules.
  *
  * @author vion
  *
  */
final class CSPOMCompiler(
                           private val problem: CSPOM,
                           private val constraintCompilers: IndexedSeq[ConstraintCompiler],
                           private val problemCompilers: Seq[ProblemCompiler]) extends LazyLogging {


  private def compile(): CSPOM = {

    val constraints = new VecMap[CSPOMConstraint[_]]() ++=
      problem.constraints.map(c => c.id -> c)

    val queue = new QueueSet(constraints.keys)

    def updateQueue(delta: Delta): Unit = {
      constraints ++= delta.added.view.map(c => c.id -> c)
      constraints --= delta.removed.map(c => c.id)
      queue.enqueueAll(delta.added.view.map(_.id))
    }

    while (queue.nonEmpty) {
      while (queue.nonEmpty) {
        val next = queue.dequeue()

        for {
          compiler <- constraintCompilers
          constraint <- constraints.get(next)
        } {
          updateQueue(compile(compiler, constraint))
        }
      }

      for (c <- problemCompilers) {
        updateQueue(c(problem))
      }
    }

    problem
  }



  private def compile(compiler: ConstraintCompiler, constraint: CSPOMConstraint[_]): Delta = {
    require(problem.constraintSet(constraint), {
      s"${constraint.toString(problem.displayName)} not in $problem"
    })
    CSPOMCompiler.matches += 1

    compiler.mtch(constraint, problem) match {
      case Some(data) =>
        CSPOMCompiler.compiles += 1
        logger.info(s"$compiler : ${constraint.toString(problem.displayName)}")
        compiler.compile(constraint, problem, data)
      case None => Delta()
    }

  }

}

object CSPOMCompiler {
  @Statistic
  var matches = 0
  @Statistic
  var compiles = 0
  @Statistic
  var compileTime: Quantity[Double] = Quantity(0, "")

  def compile(problem: CSPOM, compilers: Seq[Compiler]): Try[CSPOM] = {
    val pbc = new CSPOMCompiler(problem, compilers.collect {
      case c: ConstraintCompiler => c
    }
      .toIndexedSeq,
      compilers.collect {
        case c: ProblemCompiler => c
      })

    val (r, t) = StatisticsManager.measure(pbc.compile())

    compileTime = t

    r
  }

}

