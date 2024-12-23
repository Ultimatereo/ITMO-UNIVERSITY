package com.parser.generator.test;

import com.parser.generator.output.ClojureParser;
import com.parser.generator.utils.DotToPngConverter;
import com.parser.generator.utils.Tree;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.util.Scanner;

public class ClojureParserTest {
    public static void main(String[] args) {
        String[] tests =
                {"""
(ns test1)

(defn add [x y z] (+ x y z))

(print (add 1 2 3))
"""
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
        ClojureParser p = new ClojureParser();
        File dotFile = new File("graphC" + i + ".dot");
        parseAnsSaveIfComplete(p, test, dotFile);
        File pngFile = new File("graphC" + i + ".png");
        DotToPngConverter.convertDotToPng(dotFile.getPath(), pngFile.getPath());
    }

    private static void parseAnsSaveIfComplete(ClojureParser p, String test, File filePath) throws IOException {
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
