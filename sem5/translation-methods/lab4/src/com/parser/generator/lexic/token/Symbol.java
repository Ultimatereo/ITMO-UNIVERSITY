package com.parser.generator.lexic.token;

public class Symbol extends SimpleToken {
    public Symbol(String name, String value) {
        super(name, value);
    }

    public Symbol(String value) {
        super(value, value);
    }
}
