package cspom.xcsp

import cspom.compiler.ConstraintSignature
import cspom.variable.CSPOMBool
import cspom.variable.CSPOMInt
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMSeqType

object XCSPConstraintSignatures {
  def get = Seq(
    ConstraintSignature(CSPOMBool, "ne", CSPOMInt, CSPOMInt),
    ConstraintSignature(CSPOMBool, "or", CSPOMBool, CSPOMBool),
    ConstraintSignature(CSPOMTrue, "extension", CSPOMSeqType(CSPOMInt)))

}