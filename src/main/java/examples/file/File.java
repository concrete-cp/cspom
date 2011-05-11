package examples.file;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;

import cspom.CSPOM;
import cspom.CSPParseException;

public final class File {
	private static final String FILENAME = "crossword-m1-debug-05-01.xml";

	private File() {
	}

	public static void main(final String[] args) throws IOException,
			CSPParseException {
		final CSPOM cspom;
		try {
			cspom = CSPOM.load(File.class.getResource(FILENAME));

		} catch (CSPParseException e) {
            System.err
                    .println(e.getMessage() + " at line " + e.lineNumber());
			throw e;
		}
		System.out.println(cspom);
		final Writer writer = new FileWriter("fapp.gml");
		try {
			writer.append(cspom.toGML());
		} finally {
			writer.close();
		}

	}
}