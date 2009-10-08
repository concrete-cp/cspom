package examples.file;

import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;

import cspom.CSPOM;

public class File {

	public static void main(String[] args) throws ParseException,
			FileNotFoundException, IOException {
		new FileWriter("fapp.gml").append(
				CSPOM.load(File.class.getResource("fapp01-0200-0.xml"))
						.toGML()).close();
	}
}