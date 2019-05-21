package cspom.compiler

import com.typesafe.scalalogging.LazyLogging
import cspom.util.VecMap
import cspom.{CSPOM, CSPOMConstraint, Statistic, StatisticsManager}
import org.scalameter.Quantity

import scala.util.Try

sealed trait CompiledFunctions {
  def mtch(c: CSPOMConstraint[_]): Boolean
}

case object AnyFunction extends CompiledFunctions {
  def mtch(c: CSPOMConstraint[_]) = true
}

case class Functions(f: Symbol*) extends CompiledFunctions {
  def mtch(c: CSPOMConstraint[_]): Boolean = f.contains(c.function)
}

/**
  * This class implements some known useful reformulation rules.
  *
  * @author vion
  *
  */
final class CSPOMCompiler(
                           private val problem: CSPOM,
                           private val constraintCompilers: IndexedSeq[ConstraintCompiler],
                           private val problemCompilers: IndexedSeq[ProblemCompiler]) extends LazyLogging {

  private def compile(): CSPOM = {

    val constraints = new VecMap[CSPOMConstraint[_]]() ++=
      problem.constraints.map(c => c.id -> c)

    val specCompilers: Map[Symbol, Seq[Int]] = constraintCompilers.zipWithIndex
      .flatMap { case (cc, i) =>
        cc.functions match {
          case Functions(fs@_*) => fs.map(f => f -> i)
          case AnyFunction => Seq()
        }
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .withDefaultValue(Seq())

    val anyCompilers = constraintCompilers
      .zipWithIndex
      .collect {
        case (cc, i) if cc.functions == AnyFunction => i
      }

    // val toCompile = new mutable.HashMap[Int, mutable.Set[ConstraintCompiler]]()

    val queues = constraintCompilers.map { cc =>
      new QueueSet(cc.functions match {
        case Functions(fs@_*) => fs.flatMap(problem.getConstraints).map(_.id)
        case AnyFunction => problem.constraints.map(_.id).toIterable
      })
    }

    val compilerQueue = new QueueSet(constraintCompilers.indices)
    var pcQueue = new QueueSet(problemCompilers.indices)

    /**
      * Updates the constraint queue
      *
      * @param delta
      * @return True iff delta was not empty
      */
    def updateQueue(delta: Delta): Seq[Int] = {
      constraints ++= delta.added.view.map(c => c.id -> c)
      constraints --= delta.removed.map(c => c.id)

      for (c <- delta.added; cc <- specCompilers(c.function) ++ anyCompilers) yield {
        queues(cc).enqueue(c.id)
        cc
      }
    }

    while (compilerQueue.nonEmpty || pcQueue.nonEmpty) {
      var change = false
      while (compilerQueue.nonEmpty) {
        val i = compilerQueue.dequeue()
        val queue = queues(i)
        val compiler = constraintCompilers(i)
        while (queue.nonEmpty) {
          val next = queue.dequeue()
          for {
            constraint <- constraints.get(next)
          } {
            val delta = compile(compiler, constraint)
            compilerQueue.enqueueAll(updateQueue(delta))
            change |= delta.nonEmpty
          }
        }
      }

      if (change) {
          pcQueue.enqueueAll(problemCompilers.indices)
      }

      while (pcQueue.nonEmpty) {
        val i = pcQueue.dequeue()
        val compiler = problemCompilers(i)
        val delta = compiler(problem)
        compilerQueue.enqueueAll(updateQueue(delta))
        if (delta.nonEmpty) {
          pcQueue.enqueueAll(problemCompilers.indices.filter(_ != i))
        }
      }
    }
    problem
  }


  private def compile(compiler: ConstraintCompiler, constraint: CSPOMConstraint[_]): Delta = {
    assert(problem.hasConstraint(constraint), {
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
      }
        .toIndexedSeq)

    val (r, t: Quantity[Double]) = StatisticsManager.measure(pbc.compile())

    compileTime = t

    r
  }

}

