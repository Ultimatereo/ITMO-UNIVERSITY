import lexic.LexicalAnalyzer;
import lexic.token.token.TokenEnum;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;

public class Main {
    public static void main(String[] args) {
        String[] tests =
                {
                        "public inline fun aboba(a: Int, b: Int, c: String) -> Unit",
                        "public inline fun aboba(a: Int, b: Int, c: String)",
                        "fun a(d: Int)",
                        "fun fun(a: Int)",
                        "protected fun       hey(a: CustomClass, b: CustomClass) -> CustomClass",
                        "fun <a, b> greatFunction(element1 : ArrayList<a>, function1 : BiFunction<a, b, *>, element2: List<Int>) : Boolean"
                };
        for (int i = 0; i < tests.length; i++) {
            try {
                //        testLexic(test);
                testParse(tests[i], i);
            } catch (IOException | IllegalStateException e) {
                System.out.println(e.getMessage());
            }
        }
    }

    private static void testLexic(String test) throws ParseException {
        InputStream is = new ByteArrayInputStream(StandardCharsets.UTF_8.encode(test).array());
        LexicalAnalyzer la = new LexicalAnalyzer(is);
        while (true) {
            la.nextToken();
            TokenEnum token = la.curToken();
            System.out.println("Parsed " + token + ": " + token.token().value());
            if (token == TokenEnum.END) {
                break;
            }
        }
    }

    private static void testParse(String test, int i) throws IOException {
        Parser p = new Parser();
        File dotFile = new File("graph" + i + ".dot");
        parseAnsSaveIfComplete(p, test, dotFile);
        File pngFile = new File("graph" + i + ".png");
        DotToPngConverter.convertDotToPng(dotFile.getPath(), pngFile.getPath());
    }

    private static void parseAnsSaveIfComplete(Parser p, String test, File filePath) throws IOException {
        try {
            Tree t = p.parse(new ByteArrayInputStream(StandardCharsets.UTF_8.encode(test).
                    array()));
            try (BufferedWriter writer = new BufferedWriter(new FileWriter(filePath))) {
                writer.write("digraph ParserResult {\n");
                t.writeUsing(writer);
                writer.write("}");
            }
            System.out.println("\"" + test + "\"" + " is parsed successfully.\n");
        } catch (ParseException e) {
            System.out.println("Error happened during parsing expression: \"" + test + "\"");
            System.out.println(e.getMessage() + "\n");
        }
    }
}
