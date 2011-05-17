package cspom.variable;

trait BooleanDomain extends CSPOMDomain[Boolean] {
  def getBoolean: Boolean;
  def isConstant: Boolean;
}

object BooleanDomain {
  def valueOf(constant: Boolean): BooleanDomain = constant match {
    case true => TrueDomain;
    case false => FalseDomain;
  }
}

object UnknownBooleanDomain extends BooleanDomain {
  override def getBoolean = throw new UnsupportedOperationException("only legal on non-constant Boolean instances");
  override def isConstant = false;
  override def toString = "(false, true)"
  val values = List(false, true)
  override val size = 2
  override def intersect[Boolean](domain: CSPOMDomain[Boolean]) = 
    UnknownBooleanDomain.asInstanceOf[CSPOMDomain[Boolean]]

}

object TrueDomain extends BooleanDomain {
  override def getBoolean = true;
  override def isConstant = true;
  override def toString = "true";
  override val values = List(true);
  override val size = 1
  def intersect[Boolean](domain: CSPOMDomain[Boolean]): CSPOMDomain[Boolean] = {
    if (domain == this) {
      TrueDomain.asInstanceOf[CSPOMDomain[Boolean]]
    } else {
      UnknownBooleanDomain.asInstanceOf[CSPOMDomain[Boolean]]
    }
  }
}

object FalseDomain extends BooleanDomain {
  def getBoolean = false;
  def isConstant = true;
  override def toString = "false"
  override val values = List(false);
  override val size = 1
  def intersect[Boolean](domain: CSPOMDomain[Boolean]): CSPOMDomain[Boolean] = {
    if (domain == this)
      FalseDomain.asInstanceOf[CSPOMDomain[Boolean]]
    else
      UnknownBooleanDomain.asInstanceOf[CSPOMDomain[Boolean]]
  }
}