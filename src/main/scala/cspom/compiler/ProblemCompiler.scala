package cspom.compiler;

import scala.collection.mutable.LinkedHashMap
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.Statistic
import cspom.variable.CSPOMExpression
import scala.collection.mutable.HashMap
import cspom.StatisticsManager
import cspom.TimedException
import cspom.VariableNames

/**
 * This class implements some known useful reformulation rules.
 *
 * @author vion
 *
 */
final class ProblemCompiler(
  private val problem: CSPOM,
  private val constraintCompilers: IndexedSeq[ConstraintCompiler]) {

  val vn = new VariableNames(problem)

  private def compile() {
    val toCompile = Array.ofDim[QueueSet](
      constraintCompilers.size)

    val constraints = new HashMap[Int, CSPOMConstraint[_]]

    for (c <- problem.constraints) {
      constraints.put(c.id, c)
    }

    var changed = true
    var first = true

    while (changed) {
      changed = false
      for (i <- toCompile.indices) {

        val compiler = constraintCompilers(i)
        //println(compiler)
        if (first) {
          toCompile(i) = new QueueSet(constraints.keys)
        }

        while (toCompile(i).nonEmpty) {

          val constraint = constraints(toCompile(i).dequeue())
          ProblemCompiler.matches += 1
          //println(compiler, constraint.id)
          //print(constraint)
          for (data <- compiler.mtch(constraint, problem)) {
            ProblemCompiler.compiles += 1
            changed = true
            //print(compiler + " : " + constraint.toString(vn) + " -> ")
            val delta = compiler.compile(constraint, problem, data)

            //print(" match")

            val enqueue = delta.altered.iterator.flatMap(problem.constraints).toList

            for (rc <- delta.removed) {
              constraints.remove(rc.id)
            }

            for (ac <- enqueue) {
              constraints.put(ac.id, ac)
            }

            for (j <- if (first) { 0 to i } else { toCompile.indices }) {
              for (rc <- delta.removed) {
                toCompile(j).remove(rc.id)
              }

              if (j != i || compiler.selfPropagation) {
                for (ac <- enqueue) {
                  toCompile(j).enqueue(ac.id)
                }
              }
            }
          }
          toCompile(i).remove(constraint.id)
          //println
        }

      }
      first = false
    }
    //require(constraints.values.toSet == problem.constraints)
  }

  // println(problem)

}

object ProblemCompiler {
  def compile(problem: CSPOM, compilers: Seq[ConstraintCompiler]) {
    val pbc = new ProblemCompiler(problem, compilers.toIndexedSeq)

    val (_, t) = try StatisticsManager.time(pbc.compile())
    catch {
      case e: TimedException =>
        compileTime += e.time
        throw e.getCause()
    }
    compileTime += t
  }

  @Statistic
  var matches = 0

  @Statistic
  var compiles = 0

  @Statistic
  var compileTime = 0.0

}

