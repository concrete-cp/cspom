package cspom.variable;
import org.junit.Test
import org.junit.Before
import org.junit.Assert._

final class ExtensiveDomainTest {

    var intDomain: ExtensiveDomain[Int]=null;

    var anyDomain: ExtensiveDomain[_] = null;

    @Before
    def setUp() {
        intDomain = ExtensiveDomain.of(1, 7, 9);
        anyDomain = ExtensiveDomain.of(8.9d, 1, intDomain);
    }

    @Test
    def testGetValues() {
        assertEquals(intDomain.values, List(1, 7, 9));
    }

    @Test
    def testEquals() {
        assertEquals(intDomain, ExtensiveDomain.of(1, 7, 9));
    }

    @Test
    def testToString() {
        assertEquals(intDomain.toString, "{1, 7, 9}");
        assertEquals(anyDomain.toString, "{8.9, 1, {1, 7, 9}}");
    }
}
