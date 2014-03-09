package cspom.variable;
import org.junit.Test
import org.junit.Before
import org.junit.Assert._

final class ExtensiveDomainTest {

  var intDomain: IntDomain = null;

  //var anyDomain: ExtensiveDomain[_] = null;

  @Before
  def setUp() {
    intDomain = IntDomain(Seq(1, 7, 9));
    //anyDomain = ExtensiveDomain.of(8.9d, 1, intDomain);
  }

  @Test
  def testEquals() {
    assertEquals(List(1, 7, 9), intDomain);
  }


  @Test
  def testToString() {
    assertEquals("{1, 7, 9}", intDomain.toString);
    //assertEquals(anyDomain.toString, "{8.9, 1, {1, 7, 9}}");
  }
}
