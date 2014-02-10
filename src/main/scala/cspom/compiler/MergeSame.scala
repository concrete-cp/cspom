package cspom.compiler
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint

/**
 * Find and merge auxiliary variables similary defined by other constraints
 */
object MergeSame extends ConstraintCompiler {

  type A = CSPOMConstraint

  override def mtch(c: CSPOMConstraint, problem: CSPOM): Option[A] = {
    c.fullScope.iterator.flatMap(problem.constraints).filter(_ ne c).collectFirst {
      case same @ CSPOMConstraint(_, c.function, c.arguments, c.params) => same
    }
  }

  def compile(c: CSPOMConstraint, problem: CSPOM, same: CSPOMConstraint) = {

    val eqC = new CSPOMConstraint('eq, c.result, same.result)
    replaceCtr(c, eqC, problem)

  }
  
  def selfPropagation = false

}
