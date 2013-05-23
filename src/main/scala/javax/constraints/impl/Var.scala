package javax.constraints.impl

import cspom.variable.CSPOMDomain
import cspom.variable.CSPOMVariable

class Var(problem: Problem, name: String, val variable: CSPOMVariable) extends AbstractVar(problem, name) {
  // Members declared in javax.constraints.impl.AbstractVar 
  def addPropagator(x$1: javax.constraints.extra.Propagator, x$2: javax.constraints.extra.PropagationEvent) {
    throw new UnsupportedOperationException
  }

  // Members declared in javax.constraints.Var 
  def abs(): javax.constraints.Var = {
    val cspomVar = problem.cspom.is("abs", variable)
    new Var(problem, cspomVar.name, cspomVar)
  }
  
  def contains(x: Int): Boolean = variable.domain.contains(x)
  
  def getMax(): Int = variable.domain.values.last.asInstanceOf[Int]
  def getMin(): Int = variable.domain.values.head.asInstanceOf[Int]
  
  def isBound(): Boolean = ???
  def multiply(x$1: javax.constraints.Var): javax.constraints.Var = ???
  def multiply(x$1: Int): javax.constraints.Var = ???
  def plus(x$1: javax.constraints.Var): javax.constraints.Var = ???
  def plus(x$1: Int): javax.constraints.Var = ???
}