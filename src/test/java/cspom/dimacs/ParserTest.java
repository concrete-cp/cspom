package cspom.dimacs;

import java.io.IOException;

import org.junit.Test;

import cspom.CSPOM;
import cspom.CSPParseException;

public class ParserTest {

	private static final String FILENAME = "flat30-1.cnf";

	@Test
	public void test() throws IOException, CSPParseException {
		final CSPOM cspom = new CSPOM();
		final CNFParser parser = new CNFParser(cspom);
		parser.parse(ParserTest.class.getResource(FILENAME).openStream());
		System.out.println(cspom);
	}

}
