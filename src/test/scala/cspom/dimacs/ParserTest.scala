package cspom.dimacs;

import cspom.CSPOM
import org.junit.Assert._
import org.junit.Test

final class ParserTest {

	val FILENAME = "flat30-1.cnf";

	@Test
	def test()  {
		val cspom = new CSPOM();
		val parser = new CNFParser(cspom);
		parser.parse(classOf[ParserTest].getResourceAsStream(FILENAME));
		assertEquals(90, cspom.variables.size)
		assertEquals(300, cspom.constraints.size)
		//println(cspom)
	}

}
