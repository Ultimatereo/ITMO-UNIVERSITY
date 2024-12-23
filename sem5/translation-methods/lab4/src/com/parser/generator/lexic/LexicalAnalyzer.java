package com.parser.generator.lexic;

import com.parser.generator.lexic.factory.TokenRegexFactory;
import com.parser.generator.lexic.parse.TokenParseTree;
import com.parser.generator.lexic.parse.TrieNode;
import com.parser.generator.lexic.token.SimpleToken;
import com.parser.generator.lexic.token.Symbol;
import com.parser.generator.lexic.token.Token;

import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class LexicalAnalyzer {
    private final static Token end = new Token("$", "");
    private final InputStream is;
    private final List<TokenRegexFactory> tokenRegexFactories;
    private final TrieNode root;
    private final Set<Character> startsOfSymbols;
    private int curChar;
    private int curPos;
    private Token curToken;

    public LexicalAnalyzer(InputStream is, List<TokenRegexFactory> tokenRegexFactories, List<SimpleToken> tokens) throws ParseException {
        this.is = is;
        this.tokenRegexFactories = tokenRegexFactories;
        this.root = new TokenParseTree(tokens).root();
        this.startsOfSymbols = createStartsOfSymbols(tokens);
        curPos = 0;
        nextChar();
    }

    private Set<Character> createStartsOfSymbols(List<SimpleToken> tokens) {
        Set<Character> set = new HashSet<>();
        for (Token token : tokens) {
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
        if (curChar == 0 || curChar == -1) {
            curToken = end;
            return;
        }
        StringBuilder sb = new StringBuilder();
        if (!tryAddSymbolOrKeyWord(sb, root)) {
            while (curChar != 0 && curChar != -1 && !isBlank(curChar) && !isStartOfSymbol(curChar)) {
                sb.append((char) curChar);
                nextChar();
            }
            if (!tryAddUsingFactory(sb)) {
                throw new ParseException("Cannot parse token: " + sb, curPos);
            }
        }
    }

    private boolean tryAddUsingFactory(StringBuilder sb) {
        String s = sb.toString();
        for (TokenRegexFactory tokenRegexFactory : tokenRegexFactories) {
            Token token = tokenRegexFactory.createToken(s);
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
                return curToken instanceof Symbol || curChar == -1 || curChar == 0 || isBlank(curChar) || isStartOfSymbol(curChar);
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

    public Token curToken() {
        return curToken;
    }
}
