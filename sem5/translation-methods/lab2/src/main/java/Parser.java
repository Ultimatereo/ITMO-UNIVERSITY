import java.io.InputStream;
import java.text.ParseException;

public class Parser {
    LexicalAnalyzer lex;

    Tree S() throws ParseException {
        switch (lex.curToken()) {
            case LPAREN:
                // (
                lex.nextToken();
                // S
                Tree sub = S();
                // )
                if (lex.curToken() != Token.RPAREN) {
                    throw new ParseException(") expected at position " + lex.curPos(), lex.curPos());
                }
                lex.nextToken();
                // S ’
                Tree cont = SPrime();
                return new Tree("S", new Tree("("), sub, new Tree(")"), cont);
            case RPAREN:
            case END:
                // eps
                return new Tree("S");
            default:
                throw new AssertionError();
        }
    }

    Tree SPrime() throws ParseException {
        switch (lex.curToken()) {
            case LPAREN:
                // S
                Tree sub = S();
                // S ’
                Tree cont = SPrime();
                return new Tree("S’", sub, cont);
            case RPAREN:
            case END:
                // eps
                return new Tree("S’");
            default:
                throw new AssertionError();
        }
    }

    Tree parse(InputStream is) throws ParseException {
        lex = new LexicalAnalyzer(is);
        lex.nextToken();
        return S();
    }
}
