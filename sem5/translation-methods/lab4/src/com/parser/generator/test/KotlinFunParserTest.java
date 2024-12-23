package com.parser.generator.test;

import com.parser.generator.output.KotlinFunParser;
import com.parser.generator.utils.DotToPngConverter;
import com.parser.generator.utils.Tree;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.util.Scanner;

public class KotlinFunParserTest {
    public static void main(String[] args) {
        String[] tests =
                {
                        "fun aboba(a: Int, b: Int, c: String) -> Unit",
                        "fun aboba(a: Int, b: Int, c: String)",
                        "fun a(d: Int)",
                        "fun fun(a: Int)",
                };
        for (int i = 0; i < tests.length; i++) {
            try {
                testParse(tests[i], i);
            } catch (IOException | IllegalStateException e) {
                System.out.println(e.getMessage());
            }
        }

        Scanner sc = new Scanner(System.in);
        int i = 10;
        while (true) {
            String line = sc.nextLine();
            try {
                testParse(line, i);
                i++;
            } catch (IOException | IllegalStateException e) {
                System.out.println(e.getMessage());
            }
        }
    }

    private static void testParse(String test, int i) throws IOException {
        KotlinFunParser p = new KotlinFunParser();
        File dotFile = new File("graphK" + i + ".dot");
        parseAnsSaveIfComplete(p, test, dotFile);
        File pngFile = new File("graphK" + i + ".png");
        DotToPngConverter.convertDotToPng(dotFile.getPath(), pngFile.getPath());
    }

    private static void parseAnsSaveIfComplete(KotlinFunParser p, String test, File filePath) throws IOException {
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
