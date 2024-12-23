package com.parser.generator.rule;

import java.util.List;

public class Rule {
    private final NonTerminal leftPart;
    private final List<Element> rightPart;

    private final String semantics;

    public Rule(String semantics, NonTerminal leftPart, List<Element> rightPart) {
        this.leftPart = leftPart;
        this.rightPart = rightPart;
        this.semantics = semantics;
    }
    public Rule(String semantics, NonTerminal leftPart, Element... rightPart) {
        this.leftPart = leftPart;
        this.semantics = semantics;
        this.rightPart = List.of(rightPart);
    }

    public Rule(NonTerminal leftPart, Element... rightPart) {
        this.leftPart = leftPart;
        this.semantics = null;
        this.rightPart = List.of(rightPart);
    }

    public Rule(NonTerminal leftPart, List<Element> rightPart) {
        this.leftPart = leftPart;
        this.semantics = null;
        this.rightPart = rightPart;
    }

    @Override
    public String toString() {
        String rightPartString = getRightPartString();
        return leftPart.name() + " -> " + rightPartString;
    }

    private String getRightPartString() {
        StringBuilder sb = new StringBuilder();
        for (Element element : rightPart) {
            sb.append(element.name()).append(" ");
        }
        return sb.toString();
    }

    public NonTerminal leftPart() {
        return leftPart;
    }

    public List<Element> rightPart() {
        return rightPart;
    }

    public String semantics() {
        return semantics;
    }
}

