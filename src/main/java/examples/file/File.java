package examples.file;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.text.ParseException;

import cspom.CSPOM;

public final class File {
    private File() {
    }

    public static void main(String[] args) throws IOException, ParseException {
        final CSPOM cspom;
        try {
            cspom = CSPOM.load(File.class.getResource("queens-12.xml"));

        } catch (ParseException e) {
            System.err.println(e.getMessage() + " at line "
                    + e.getErrorOffset());
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