package cspom.xcsp

import cspom.{CSPOM, CSPOMConstraint}
import cspom.variable.CSPOMSeq
import org.xcsp.parser.entries.XVariables.XVarInteger

/**
  * Created by vion on 30/05/17.
  */
trait XCSP3CallbacksPackSched extends XCSP3CallbacksVars {


  override def buildCtrNoOverlap(id: String,
                                 origins: Array[XVarInteger],
                                 lengths: Array[Int],
                                 zeroIgnored: Boolean) {
    buildCtrNoOverlap(cspomSeq(origins), CSPOM.constantSeq(lengths), zeroIgnored)
  }

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[XVarInteger],
                                 lengths: Array[XVarInteger],
                                 zeroIgnored: Boolean) {
    buildCtrNoOverlap(cspomSeq(origins), cspomSeq(lengths), zeroIgnored)
  }

  private def buildCtrNoOverlap(origins: CSPOMSeq[Int], lengths: CSPOMSeq[Int], zeroIgnored: Boolean): Unit = {
    cspom.ctr {
      CSPOMConstraint('noOverlap)(origins, lengths) withParam "zeroIgnored" -> zeroIgnored
    }
  }

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[Array[XVarInteger]],
                                 lengths: Array[Array[Int]],
                                 zeroIgnored: Boolean) {
    unimplementedCase(id)
  }

  override def buildCtrNoOverlap(id: String,
                                 origins: Array[Array[XVarInteger]],
                                 lengths: Array[Array[XVarInteger]],
                                 zeroIgnored: Boolean) {
    unimplementedCase(id)
  }

}
