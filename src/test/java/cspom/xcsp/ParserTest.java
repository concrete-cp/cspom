package cspom.xcsp;

import java.io.IOException;

import org.junit.Test;

import cspom.CSPOM;
import cspom.CSPParseException;

public class ParserTest {

    private static final String FILENAME = "crossword-m1-debug-05-01.xml";

    @Test
    public void test() throws CSPParseException, IOException {
        final CSPOM cspom = new CSPOM();
        final XCSPParser parser = new XCSPParser(cspom);
        parser.parse(ParserTest.class.getResource(FILENAME).openStream());

    }


}
