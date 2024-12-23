package lexic.token.parse;

import lexic.token.token.TokenEnum;

import java.util.HashMap;
import java.util.Map;

public class TrieNode {
    private final Map<Character, TrieNode> children;
    private TokenEnum token;

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

    public TokenEnum token() {
        return token;
    }

    public void setToken(TokenEnum token) {
        this.token = token;
    }
}
