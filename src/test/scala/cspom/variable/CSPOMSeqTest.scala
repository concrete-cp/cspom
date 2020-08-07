package cspom.variable

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author vion
 */
class CSPOMSeqTest extends AnyFlatSpec with Matchers with OptionValues {
  "collectAll" should "not alter sequence order" in {
    val l = IndexedSeq(1, 2, 3)

    val c = CSPOMSeq.collectAll(l) {
      case i: Int => i
    }

    c.value should contain theSameElementsInOrderAs l
  }
}