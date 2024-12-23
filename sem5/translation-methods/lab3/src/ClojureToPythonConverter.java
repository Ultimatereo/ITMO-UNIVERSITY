import antlr.parser.ClojureLexer;
import antlr.parser.ClojureParser;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class ClojureToPythonConverter {
    public static void main(String[] args) {
        File folder = new File("D:\\KT\\MT\\lab3\\test");
        if (folder.exists() && folder.isDirectory()) {
            File[] files = folder.listFiles();
            assert files != null;
            for (File file : files) {
                if (file.isFile()) {
                    try {
                        BufferedReader reader = new BufferedReader(new FileReader(file));
                        StringBuilder content = new StringBuilder();
                        String line;
                        while ((line = reader.readLine()) != null) {
                            content.append(line);
                            content.append(System.lineSeparator());
                        }
                        reader.close();
                        System.out.println("File: " + file.getName());
                        System.out.println("Clojure code:");
                        System.out.println(content);
                        System.out.println("-------------------------");
                        System.out.println("Python code:");
                        test(content.toString());
                        System.out.println("-------------------------");
                    } catch (IOException e) {
                        System.out.println(e.getMessage());
                    }
                }
            }
        } else {
            System.out.println("Папка не существует или не является папкой.");
        }
    }

    private static void test(String input) {
        ClojureLexer lexer = new ClojureLexer(CharStreams.fromString(input));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        ClojureParser parser = new ClojureParser(tokens);
        ParseTree tree = parser.program();

        // Создание посетителя и применение его к дереву разбора
        ClojureToPythonVisitor visitor = new ClojureToPythonVisitor();
        String pythonCode = visitor.visit(tree);

        System.out.println(pythonCode);
    }


}

