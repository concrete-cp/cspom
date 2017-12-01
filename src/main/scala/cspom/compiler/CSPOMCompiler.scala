package cspom.compiler;

import scala.util.Try
import com.typesafe.scalalogging.LazyLogging
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.Statistic
import cspom.StatisticsManager
import org.scalameter.Quantity
import cspom.util.VecMap

/**
 * This class implements some known useful reformulation rules.
 *
 * @author vion
 *
 */
final class CSPOMCompiler(
    private val problem: CSPOM,
    private val constraintCompilers: IndexedSeq[ConstraintCompiler]) extends LazyLogging {


  private def compile(): CSPOM = {

    val constraints = new VecMap[CSPOMConstraint[_]]() ++=
      problem.constraints.map(c => c.id -> c)

    val queue = new QueueSet(constraints.keys)

    while (queue.nonEmpty) {

      val next = queue.dequeue()

      for {
        compiler <- constraintCompilers
        constraint <- constraints.get(next)
      } {
        //println(s"$next, $compiler")
        //println(s"Compiling ${constraint.toString(vn)} with $compiler")
        //lazy val string = constraint.toString(vn)
        val delta = compile(compiler, constraint)

        //println(delta)

        constraints ++= delta.added.view.map(c => c.id -> c)
        constraints --= delta.removed.map(c => c.id)

        queue.enqueueAll(delta.added.view.map(_.id))

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
  def compile(problem: CSPOM, compilers: Seq[ConstraintCompiler]): Try[CSPOM] = {
    val pbc = new CSPOMCompiler(problem, compilers.toIndexedSeq)

    val (r, t) = StatisticsManager.measure(pbc.compile())

    compileTime = t
    
    r
  }

  @Statistic
  var matches = 0

  @Statistic
  var compiles = 0

  @Statistic
  var compileTime: Quantity[Double] = Quantity(0, "")

}

