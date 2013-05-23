package javax.constraints.impl

import cspom.CSPOM

class Problem(name: String = "") extends AbstractProblem(name) {
  val cspom = new CSPOM()
  // Members declared in javax.constraints.impl.AbstractProblem
  protected def createSolver(): javax.constraints.Solver =
    throw new UnsupportedOperationException("CSPOM does not depend on any solver")
  
  def createVariable(x$1: String, x$2: Int, x$3: Int): javax.constraints.Var = ???
  def debug(x$1: String): Unit = ???
  def error(x$1: String): Unit = ???
  def getImplVersion(): String = ???
  def log(x$1: String): Unit = ???
  def post(x$1: javax.constraints.Constraint): Unit = ???
  def variableBool(x$1: String): javax.constraints.VarBool = ???

  // Members declared in javax.constraints.Problem
  def allDiff(x$1: Array[javax.constraints.Var]): javax.constraints.Constraint = ???
  def linear(x$1: javax.constraints.Var, x$2: String, x$3: javax.constraints.Var): javax.constraints.Constraint = ???
  def linear(x$1: javax.constraints.Var, x$2: String, x$3: Int): javax.constraints.Constraint = ???
  def loadFromXML(x$1: java.io.InputStream): Unit = ???
  def post(x$1: javax.constraints.Var, x$2: String, x$3: javax.constraints.Var): javax.constraints.Constraint = ???
  def post(x$1: javax.constraints.Var, x$2: String, x$3: Int): javax.constraints.Constraint = ???
  def post(x$1: Array[javax.constraints.Var], x$2: String, x$3: javax.constraints.Var): javax.constraints.Constraint = ???
  def post(x$1: Array[javax.constraints.Var], x$2: String, x$3: Int): javax.constraints.Constraint = ???
  def post(x$1: Array[Int], x$2: Array[javax.constraints.Var], x$3: String, x$4: javax.constraints.Var): javax.constraints.Constraint = ???
  def post(x$1: Array[Int], x$2: Array[javax.constraints.Var], x$3: String, x$4: Int): javax.constraints.Constraint = ???
  def postCardinality(x$1: Array[javax.constraints.Var], x$2: Int, x$3: String, x$4: javax.constraints.Var): javax.constraints.Constraint = ???
  def postCardinality(x$1: Array[javax.constraints.Var], x$2: Int, x$3: String, x$4: Int): javax.constraints.Constraint = ???
  def postElement(x$1: Array[javax.constraints.Var], x$2: javax.constraints.Var, x$3: String, x$4: javax.constraints.Var): javax.constraints.Constraint = ???
  def postElement(x$1: Array[javax.constraints.Var], x$2: javax.constraints.Var, x$3: String, x$4: Int): javax.constraints.Constraint = ???
  def postElement(x$1: Array[Int], x$2: javax.constraints.Var, x$3: String, x$4: javax.constraints.Var): javax.constraints.Constraint = ???
  def postElement(x$1: Array[Int], x$2: javax.constraints.Var, x$3: String, x$4: Int): javax.constraints.Constraint = ???
  def scalProd(x$1: Array[Int], x$2: Array[javax.constraints.Var]): javax.constraints.Var = ???
  def storeToXML(x$1: java.io.OutputStream, x$2: String): Unit = ???

}