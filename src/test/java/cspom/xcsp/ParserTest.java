package cspom.xcsp;

import java.io.IOException;

import org.junit.Test;

import cspom.CSPOM;

public class ParserTest {

	private static final String FILENAME = "crossword-m1-debug-05-01.xml";

	@Test
	public void test() throws XCSPParseException, IOException {
		final CSPOM cspom = new CSPOM();
		final Parser parser = new Parser(cspom);
		parser.parse(ParserTest.class.getResource(FILENAME).openStream());

	}

}
