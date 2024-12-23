package com.parser.generator.lexic.factory;

import com.parser.generator.lexic.token.Token;

import java.util.regex.Pattern;

public class TokenRegexFactory extends Token {
    private final String name;
    private final Pattern pattern;
    private final String regex;

    public TokenRegexFactory(String name, String regex) {
        super(name);
        this.name = name;
        this.pattern = Pattern.compile(regex);
        this.regex = regex;
    }

    public String regex() {
        return regex;
    }

    public String name() {
        return name;
    }

    public boolean check(String val) {
        return pattern.matcher(val).matches();
    }

    public Token createToken(String val) {
        if (!check(val)) {
            return null;
        }
        return new Token(name, val);
    }
}
