package lexic;

import lexic.token.factory.TokenFactoryEnum;
import lexic.token.factory.TokenRegexFactory;
import lexic.token.parse.TokenParseTree;
import lexic.token.parse.TrieNode;
import lexic.token.token.Symbol;
import lexic.token.token.Token;
import lexic.token.token.TokenEnum;

import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
import java.util.HashSet;
import java.util.Set;

public class LexicalAnalyzer {
    private final InputStream is;
    private final Set<Character> startsOfSymbols;
    private final TrieNode root;
    private int curChar;
    private int curPos;
    private TokenEnum curToken;

    public LexicalAnalyzer(InputStream is) throws ParseException {
        this.is = is;
        curPos = 0;
        nextChar();
        startsOfSymbols = createStartsOfSymbols(TokenEnum.values());
        root = new TokenParseTree().root();
    }

    public static void main(String[] args) {

    }

    private Set<Character> createStartsOfSymbols(TokenEnum[] values) {
        Set<Character> set = new HashSet<>();
        for (TokenEnum value : values) {
            Token token = value.token();
            if (token instanceof Symbol) {
                set.add(token.value().charAt(0));
            }
        }
        return set;
    }

    private boolean isBlank(int c) {
        return Character.isWhitespace((char) c);
    }

    private boolean isStartOfSymbol(int c) {
        return startsOfSymbols.contains((char) c);
    }

    private void nextChar() throws ParseException {
        curPos++;
        try {
            curChar = is.read();
        } catch (IOException e) {
            throw new ParseException(e.getMessage(), curPos);
        }
    }

    public void nextToken() throws ParseException {
        while (isBlank(curChar)) {
            nextChar();
        }
        parse();
    }

    private void parse() throws ParseException {
        if (curChar == 0) {
            curToken = TokenEnum.END;
            return;
        }
        StringBuilder sb = new StringBuilder();
        if (!tryAddSymbolOrKeyWord(sb, root)) {
            while (curChar != 0 && !isBlank(curChar) && !isStartOfSymbol(curChar)) {
                sb.append((char) curChar);
                nextChar();
            }
            if (!tryAddNameOrValue(sb)) {
                throw new ParseException("Cannot parse token: " + sb, curPos);
            }
        }
    }

    private boolean tryAddNameOrValue(StringBuilder sb) {
        String s = sb.toString();
        for (TokenFactoryEnum tokenFactoryEnum : TokenFactoryEnum.values()) {
            TokenRegexFactory tokenFactory = tokenFactoryEnum.tokenFactory();
            TokenEnum token = tokenFactory.createToken(s);
            if (token != null) {
                curToken = token;
                return true;
            }
        }
        return false;
    }

    private boolean tryAddSymbolOrKeyWord(StringBuilder sb, TrieNode root) throws ParseException {
        TrieNode curNode = root;
        while (true) {
            char c = (char) curChar;
            if (curNode.isEnd()) {
                curToken = curNode.token();
                return curToken.token() instanceof Symbol || curChar == 0 || isBlank(curChar) || isStartOfSymbol(curChar);
            }
            if (curNode.children().containsKey(c)) {
                curNode = curNode.children().get(c);
            } else {
                return false;
            }
            sb.append(c);
            nextChar();
        }
    }

    public TokenEnum curToken() {
        return curToken;
    }
}
