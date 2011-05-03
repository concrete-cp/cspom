package cspom.variable;

trait BooleanDomain extends CSPOMDomain[Boolean] {
  def getBoolean: Boolean;
  def isConstant: Boolean;

}

object BooleanDomain extends BooleanDomain {
  override def getBoolean = throw new UnsupportedOperationException("only legal on non-constant Boolean instances");
  override def isConstant = false;
  override def toString = "(false, true)"
  override val getValues = List(false, true)
  override def getSize = 2
  override def intersect(domain: CSPOMDomain[Boolean]) = this

  def valueOf(constant: Boolean): BooleanDomain = constant match {
    case true => TrueDomain;
    case false => FalseDomain;
  }
}

object TrueDomain extends BooleanDomain {
  override def getBoolean = true;
  override def isConstant = true;
  override def toString = "true";
  override val getValues = List(true);
  override def getSize = 1
  def intersect(domain: CSPOMDomain[Boolean]) = {
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
  override val getValues = List(false);
  override def getSize = 1
  override def intersect(domain: CSPOMDomain[Boolean]) = {
    if (domain == this)
      this
    else
      BooleanDomain
  }
}