package com.parser.generator.lexic.token;

import com.parser.generator.rule.Element;

public class Token implements Element {
    private final String name;
    private String value;
    public Token(String name) {
        this.name = name;
    }

    public Token(String name, String value) {
        this.name = name;
        this.value = value;
    }

    @Override
    public String toString() {
        return "Token{" +
                "name='" + name + '\'' +
                ", value='" + value + '\'' +
                '}';
    }

    public String name() {
        return name;
    }

    public String value() {
        return value;
    }
}
