package cspom.variable

import com.google.common.collect.ImmutableList
final class Constant[T](val value: T) extends CSPOMDomain[T] {
  override def hashCode = value.hashCode

  override def toString = value.toString

  def getValue = value

  def getValues = ImmutableList.of(value)

  def getSize = 1

  override def equals(that: Any) = that match {
    case other: Constant[T] => other.value == value
    case _ => false
  }

  def merge(merged: CSPOMDomain[_]): CSPOMDomain[_] = {
    if (merged.getValues.contains(value)) {
      return merged;
    }
    throw new IllegalArgumentException("Inconsistent merge");
  }
}