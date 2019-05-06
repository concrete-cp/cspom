package cspom.compiler

import com.typesafe.scalalogging.LazyLogging
import cspom.{CSPOM, CSPOMConstraint, Statistic, StatisticsManager}
import cspom.util.VecMap
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

    val queue = new QueueSet(constraints.keys)


    val specCompilers: Map[Symbol, Seq[ConstraintCompiler]] = constraintCompilers
      .flatMap(cc => cc.functions match {
        case Functions(fs@_*) => fs.map(f => f -> cc)
        case AnyFunction => Seq()
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .withDefaultValue(Seq())

    val anyCompilers = constraintCompilers.filter { cc =>
      cc.functions match {
        case AnyFunction => true
        case _ => false
      }
    }

    // val toCompile = new mutable.HashMap[Int, mutable.Set[ConstraintCompiler]]()

    val problemCompilerQueue = new QueueSet()

    /**
      * Updates the constraint queue
      * @param delta
      * @return True iff delta was not empty
      */
    def updateQueue(delta: Delta): Boolean = {
      constraints ++= delta.added.view.map(c => c.id -> c)
      constraints --= delta.removed.map(c => c.id)

      for (c <- delta.added) {
        queue.enqueue(c.id)
      }

      delta.nonEmpty
    }

    var changed = true
    while (queue.nonEmpty) {
      while (queue.nonEmpty) {
        val next = queue.dequeue()
        for {
          constraint <- constraints.get(next)
          compiler <- specCompilers(constraint.function).iterator ++ anyCompilers.iterator
          //check is to detect when the constraint is removed by some compiler
          if constraints.contains(next)
        } {
          val delta = compile(compiler, constraint)
          changed |= updateQueue(delta)
        }
      }

      if (changed) {
        problemCompilerQueue.enqueueAll(problemCompilers.indices)
        changed = false
      }
      for (pc <- problemCompilerQueue.iterator) {
        problemCompilerQueue.remove(pc)
        val c = problemCompilers(pc)
        logger.info(s"$c")
        val delta = c(problem)
        if (updateQueue(delta)) {
          for (i <- problemCompilers.indices if i != pc) {
            problemCompilerQueue.enqueue(i)
          }
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

    val (r, t) = StatisticsManager.measure(pbc.compile())

    compileTime = t

    r
  }

}

