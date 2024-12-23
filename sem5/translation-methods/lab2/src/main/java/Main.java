import java.io.*;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;

public class Main {
    public static void main(String[] args) {
        Parser p = new Parser();
        try {
            Tree t = p.parse(new ByteArrayInputStream(StandardCharsets.UTF_8.encode("(())()()((())()))").
                    array()));
            File filePath = new File("graph.dot");
            try (BufferedWriter writer = new BufferedWriter(new FileWriter(filePath))) {
                writer.write("digraph ParserResult {\n");
                t.writeUsing(writer);
                writer.write("\n");
            } catch (IOException e) {
                // Handle any potential IO exceptions
                e.printStackTrace();
            }

        } catch (ParseException e) {
            System.out.println(e.getMessage());
        }
    }
}
