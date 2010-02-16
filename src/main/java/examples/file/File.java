package examples.file;

import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;

import cspom.CSPOM;

public final class File {
	private File() {
	}

	public static void main(String[] args) throws IOException {
		try {
			new FileWriter("fapp.gml").append(
					CSPOM.load(File.class.getResource("queens-12.xml"))
							.toGML()).close();
		} catch (ParseException e) {
			System.err.println(e.getMessage() + " at line "
					+ e.getErrorOffset());
			e.printStackTrace();
		}
	}
}