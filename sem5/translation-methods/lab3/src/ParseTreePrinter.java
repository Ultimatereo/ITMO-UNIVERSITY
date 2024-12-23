import antlr.parser.ClojureLexer;
import antlr.parser.ClojureParser;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

public class ParseTreePrinter {

    public static void print(ParseTree tree) {
        print(tree, 0);
    }

    private static void print(ParseTree tree, int depth) {
        System.out.println("  ".repeat(Math.max(0, depth))
                + tree.getClass().getSimpleName() + ": " + tree.getText());

        for (int i = 0; i < tree.getChildCount(); i++) {
            print(tree.getChild(i), depth + 1);
        }
    }

    public static void main(String[] args) {
        String input = """
                (ns test9)
                                
                (let [x 1
                      y 1
                      s (+ x y)]
                  (print (* s 10))
                  )
                                
                (let [] (print 24))
                """;
        ClojureLexer lexer = new ClojureLexer(CharStreams.fromString(input));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
//        tokens.fill();
//        for (Token token: tokens.getTokens()) {
//            String tokenName = ClojureLexer.VOCABULARY.getSymbolicName(token.getType());
//            String tokenText = token.getText();
//            System.out.println(tokenName + " : " + tokenText);
//        }
        ClojureParser parser = new ClojureParser(tokens);
        ParseTree tree = parser.program();

        print(tree);
    }
}
