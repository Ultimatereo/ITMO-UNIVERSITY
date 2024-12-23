import lexic.LexicalAnalyzer;
import lexic.token.token.TokenEnum;

import java.io.InputStream;
import java.text.ParseException;
import java.util.Objects;

public class Parser {
    LexicalAnalyzer lex;

    private Tree S() throws ParseException {
        TokenEnum startToken = lex.curToken();
        lex.nextToken();
        switch (startToken) {
            case TokenEnum.INLINE, TokenEnum.INFIX, TokenEnum.PUBLIC,
                    TokenEnum.PRIVATE, TokenEnum.PROTECTED, TokenEnum.INTERNAL,
                    TokenEnum.OPEN -> {
                Tree sub = S();
                return new Tree("S", new Tree("M", new Tree(startToken.token().value())), sub);
            }
            case TokenEnum.FUN -> {
                Tree subG = G();
                Tree subFN = FN();
                assert lex.curToken() == TokenEnum.LPAREN;
                lex.nextToken();
                Tree subFA = FA();
                assert lex.curToken() == TokenEnum.RPAREN;
                lex.nextToken();
                Tree subRT = RT();
                return new Tree("S",
                        new Tree("fun"),
                        subG,
                        subFN,
                        new Tree("("),
                        subFA,
                        new Tree(")"),
                        subRT);
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }
    }

    private Tree G() throws ParseException {
        TokenEnum startToken = lex.curToken();
        if (Objects.requireNonNull(startToken) == TokenEnum.LCHEVRON) {
            lex.nextToken();
            assert lex.curToken() == TokenEnum.NAME;
            String name = lex.curToken().token().value();
            lex.nextToken();
            Tree subGC = GC();
            assert lex.curToken() == TokenEnum.RCHEVRON;
            lex.nextToken();
            return new Tree("G",
                    new Tree("<"),
                    new Tree(name),
                    subGC,
                    new Tree(">")
            );
        }
        return new Tree("G");
    }

    private Tree GC() throws ParseException {
        TokenEnum startToken = lex.curToken();
        if (Objects.requireNonNull(startToken) == TokenEnum.COMMA) {
            lex.nextToken();
            assert lex.curToken() == TokenEnum.NAME;
            String name = lex.curToken().token().value();
            lex.nextToken();
            Tree subGC = GC();
            return new Tree("GC",
                    new Tree(","),
                    new Tree(name),
                    subGC
            );
        }
        return new Tree("GC");
    }

    private Tree FA() throws ParseException {
        TokenEnum startToken = lex.curToken();
        String name = startToken.token().value();
        switch (startToken) {
            case TokenEnum.NAME -> {
                lex.nextToken();
                assert lex.curToken() == TokenEnum.COLON;
                lex.nextToken();
                Tree subT = T();
                Tree subFAC = FAC();
                return new Tree("FA",
                        new Tree(name),
                        new Tree(":"),
                        subT,
                        subFAC);
            }

            case TokenEnum.RPAREN -> {
                return new Tree("FA");
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }
    }

    private Tree FAC() throws ParseException {
        TokenEnum startToken = lex.curToken();
        lex.nextToken();
        if (Objects.requireNonNull(startToken) == TokenEnum.COMMA) {
            TokenEnum token = lex.curToken();
            String name = token.token().value();
            assert lex.curToken() == TokenEnum.NAME;
            lex.nextToken();
            assert lex.curToken() == TokenEnum.COLON;
            lex.nextToken();
            Tree subT = T();
            Tree subFAC = FAC();
            return new Tree("FAC",
                    new Tree(","),
                    new Tree(name),
                    new Tree(":"),
                    subT,
                    subFAC);
        }
        return new Tree("FAC");
    }

    private Tree FN() throws ParseException {
        TokenEnum startToken = lex.curToken();
        String name = startToken.token().value();
        lex.nextToken();
        if (Objects.requireNonNull(startToken) == TokenEnum.NAME) {
            return new Tree("FN", new Tree(name));
        }
        throw new IllegalStateException("Unexpected value: " + lex.curToken());
    }

    private Tree RT() throws ParseException {
        TokenEnum startToken = lex.curToken();
        lex.nextToken();
        if (Objects.requireNonNull(startToken) == TokenEnum.COLON) {
            Tree sub = T();
            assert lex.curToken() == TokenEnum.END;
            return new Tree("RT", new Tree(":"), sub);
        }
        assert lex.curToken() == TokenEnum.END;
        return new Tree("RT");
    }

    private Tree T() throws ParseException {
        TokenEnum startToken = lex.curToken();
        String name = startToken.token().value();
        lex.nextToken();
        if (Objects.requireNonNull(startToken) == TokenEnum.NAME) {
            Tree subGP = GP();
            return new Tree(
                    "T",
                    new Tree(name),
                    subGP
            );
        }
        throw new IllegalStateException("Unexpected value: " + lex.curToken());
    }


    private Tree GP() throws ParseException {
        TokenEnum startToken = lex.curToken();
        if (Objects.requireNonNull(startToken) == TokenEnum.LCHEVRON) {
            lex.nextToken();
            Tree subGT = GT();
            Tree subGPC = GPC();
            assert lex.curToken() == TokenEnum.RCHEVRON;
            lex.nextToken();
            return new Tree("GP",
                    new Tree("<"),
                    subGT,
                    subGPC,
                    new Tree(">")
            );
        }
        return new Tree("GP");
    }

    private Tree GPC() throws ParseException {
        TokenEnum startToken = lex.curToken();
        if (Objects.requireNonNull(startToken) == TokenEnum.COMMA) {
            lex.nextToken();
            Tree subGT = GT();
            Tree subGPC = GPC();
            return new Tree("GPC",
                    new Tree(","),
                    subGT,
                    subGPC
            );
        }
        return new Tree("GPC");
    }

    private Tree GT() throws ParseException {
        TokenEnum startToken = lex.curToken();
        switch (startToken) {
            case TokenEnum.STAR -> {
                lex.nextToken();
                return new Tree("GT", new Tree("*"));
            }
            case TokenEnum.NAME -> {
                Tree subT = T();
                return new Tree("GT", subT);
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }
    }

    Tree parse(InputStream is) throws ParseException {
        lex = new LexicalAnalyzer(is);
        lex.nextToken();
        return S();
    }
}
