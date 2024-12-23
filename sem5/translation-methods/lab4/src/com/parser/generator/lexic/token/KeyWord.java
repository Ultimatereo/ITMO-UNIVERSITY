package com.parser.generator.lexic.token;

public class KeyWord extends SimpleToken {
    public KeyWord(String value) {
        super(value.toUpperCase(), value);
    }
}
