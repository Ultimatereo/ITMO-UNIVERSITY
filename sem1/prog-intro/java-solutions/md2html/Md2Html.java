package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;

public class Md2Html {
    public static void main(String[] args) {
        String inputFile = args[0];
        String outputFile = args[1];
        StringBuilder block = new StringBuilder();
        StringBuilder answer = new StringBuilder();
        try {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(inputFile), StandardCharsets.UTF_8))) {
                while (true) {
                    String line = reader.readLine();
                    //System.err.println("LINE: " + line);
                    if (!(line == null || line.equals(""))) {
                        block.append(line).append('\n');
                    } else if (block.length() > 0) {
                        block.setLength(block.length() - 1);
                        new Parser(block).md2Html(answer);
                        //System.err.println("BLOCK: " + block);
//                        System.err.println(input);
//                        System.err.println("--------------------------------------------------------------------------");
                        block = new StringBuilder();
                    }
                    if (line == null) {
                        break;
                    }
                    //System.err.println(line);
                    //System.err.println("            NEWLINE             ");
                }
            }

            try (BufferedWriter writer = new BufferedWriter(
                    new OutputStreamWriter(new FileOutputStream(outputFile), StandardCharsets.UTF_8)
                    // We can use ARM instead of try + finally block since java 1.7 which is very useful
            )) {
                writer.write(answer.toString());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
