package cspom
import CSPOM._
import cspom.variable.IntVariable
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.typesafe.scalalogging.LazyLogging

/**
 * @author vion
 */
class GMLTest extends FlatSpec with Matchers with LazyLogging {

  "GML" should "generate proper GML" in {
    val cspom = CSPOM { implicit problem =>
      val (x, y, z) = (
        IntVariable(0 to 10) as "X",
        IntVariable(0 to 10) as "Y",
        IntVariable(0 to 10) as "Z")

      ctr(CSPOMConstraint("leq")(x, y))
      ctr(CSPOMConstraint("leq")(x, y, z))

    }

    val gml = GML(cspom)

    logger.info(gml)

    //shouldBe """graph [
    //directed 0
    //
    //          node [
    //            id "X"
    //            label "X"
    //          ]
    //          
    //          node [
    //            id "Y"
    //            label "Y"
    //          ]
    //          
    //          node [
    //            id "Z"
    //            label "Z"
    //          ]
    //          
    //          edge [
    //            source "X"
    //            target "Y"
    //            label "leq"
    //          ]
    //          
    //          node [
    //            id "cons1"
    //            label "leq"
    //            graphics [ fill "#FFAA00" ]
    //          ]
    //          
    //          edge [
    //            source "cons1"
    //            target "X"
    //          ]
    //          
    //          edge [
    //            source "cons1"
    //            target "Y"
    //          ]
    //          
    //          edge [
    //            source "cons1"
    //            target "Z"
    //          ]
    //          ]
    //"""
  }
}