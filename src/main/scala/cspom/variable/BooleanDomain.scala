package cspom.variable;

trait BooleanDomain extends CSPOMDomain[Boolean] {
  def getBoolean: Boolean;
  def isConstant: Boolean;
}

object UnknownBooleanDomain extends BooleanDomain {
  override def getBoolean = throw new UnsupportedOperationException("only legal on non-constant Boolean instances");
  override def isConstant = false;
  override def toString = "(false, true)"
  val values = List(false, true)
  override val size = 2
  override def intersect[Boolean](domain: CSPOMDomain[Boolean]) = this

  def valueOf(constant: Boolean): BooleanDomain = constant match {
    case true => TrueDomain;
    case false => FalseDomain;
  }
}

object TrueDomain extends BooleanDomain {
  override def getBoolean = true;
  override def isConstant = true;
  override def toString = "true";
  override val values = List(true);
  override val size = 1
  def intersect(domain: CSPOMDomain[Boolean]): CSPOMDomain[Boolean] = {
    if (domain == this) {
      this
    } else {
      BooleanDomain
    }
  }
}

object FalseDomain extends BooleanDomain {
  override def getBoolean = false;
  override def isConstant = true;
  override def toString = "false"
  override val values = List(false);
  override val size = 1
  override def intersect(domain: CSPOMDomain[Boolean]): CSPOMDomain[Boolean] = {
    if (domain == this)
      this
    else
      BooleanDomain
  }
}