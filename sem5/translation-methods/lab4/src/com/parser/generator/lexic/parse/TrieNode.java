package com.parser.generator.lexic.parse;

import com.parser.generator.lexic.token.Token;

import java.util.HashMap;
import java.util.Map;

public class TrieNode {
    private final Map<Character, TrieNode> children;
    private Token token;

    public TrieNode() {
        this.children = new HashMap<>();
        this.token = null;
    }

    public Map<Character, TrieNode> children() {
        return children;
    }

    public boolean isEnd() {
        return token != null;
    }

    public Token token() {
        return token;
    }

    public void setToken(Token token) {
        this.token = token;
    }
}
