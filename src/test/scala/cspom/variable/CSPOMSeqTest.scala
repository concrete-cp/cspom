package cspom.variable

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.OptionValues

/**
 * @author vion
 */
class CSPOMSeqTest extends FlatSpec with Matchers with OptionValues {
  "collectAll" should "not alter sequence order" in {
    val l = Seq(1, 2, 3)

    val c = CSPOMSeq.collectAll(l) {
      case i: Int => i
    }

    c.value should contain theSameElementsInOrderAs l
  }
}