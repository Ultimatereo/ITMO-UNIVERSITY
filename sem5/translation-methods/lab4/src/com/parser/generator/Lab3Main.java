package com.parser.generator;

import com.parser.generator.lexic.factory.TokenRegexFactory;
import com.parser.generator.lexic.token.KeyWord;
import com.parser.generator.lexic.token.SimpleToken;
import com.parser.generator.lexic.token.Symbol;
import com.parser.generator.lexic.token.Token;
import com.parser.generator.rule.NonTerminal;
import com.parser.generator.rule.Rule;

import java.util.*;

public class Lab3Main {
    private final static Map<String, NonTerminal> nts = new HashMap<>();
    private final static Map<String, Token> ts = new HashMap<>();
    private final static List<SimpleToken> simpleTokens = new ArrayList<>();
    private final static List<TokenRegexFactory> factoryTokens = new ArrayList<>();

    private final static List<Rule> rules = new ArrayList<>();

    static {
        nts.put("P", new NonTerminal("P"));
        nts.put("SF", new NonTerminal("SF"));
        nts.put("F", new NonTerminal("F"));
        nts.put("FB", new NonTerminal("FB"));
        nts.put("NS", new NonTerminal("NS"));
        nts.put("DEFN", new NonTerminal("DEFN"));
        nts.put("LET", new NonTerminal("LET"));
        nts.put("BS", new NonTerminal("BS"));
        nts.put("B", new NonTerminal("B"));
        nts.put("ES", new NonTerminal("ES"));
        nts.put("AF", new NonTerminal("AF"));
        nts.put("CF", new NonTerminal("CF"));
        nts.put("GF", new NonTerminal("GF"));
        nts.put("AI", new NonTerminal("AI"));
        nts.put("FORM", new NonTerminal("FORM"));
        nts.put("FORMM", new NonTerminal("FORMM"));
        nts.put("ATOM", new NonTerminal("ATOM"));
        nts.put("OP", new NonTerminal("OP"));

        ts.put("+", new Symbol("+"));
        ts.put("-", new Symbol("-"));
        ts.put("*", new Symbol("*"));
        ts.put("/", new Symbol("/"));

        ts.put("(", new Symbol("("));
        ts.put(")", new Symbol(")"));
        ts.put("[", new Symbol("["));
        ts.put("]", new Symbol("]"));

        ts.put("defn", new KeyWord("defn"));
        ts.put("ns", new KeyWord("ns"));
        ts.put("let", new KeyWord("let"));

        ts.put("id", new TokenRegexFactory("id", "[a-zA-Z][a-zA-Z0-9-]*"));
        ts.put("number", new TokenRegexFactory("number", "\\d+"));
        ts.put("string", new TokenRegexFactory("string", "\"(~[\"\\r\\n])*\""));

        for (String k : ts.keySet()) {
            if (ts.get(k) instanceof SimpleToken sts) {
                simpleTokens.add(sts);
            } else {
                factoryTokens.add((TokenRegexFactory) ts.get(k));
            }
        }

        rules.add(new Rule(
                nts.get("P"), nts.get("SF"), nts.get("P")
        ));


        rules.add(new Rule(
                nts.get("P"), LL1ParserGenerator.epsilon
        ));

        rules.add(new Rule(
                nts.get("SF"), nts.get("F")
        ));

        rules.add(new Rule(
                nts.get("F"), ts.get("("), nts.get("FB"), ts.get(")")
        ));

        rules.add(new Rule(
                nts.get("FB"), nts.get("NS")
        ));
        rules.add(new Rule(
                nts.get("FB"), nts.get("DEFN")
        ));
        rules.add(new Rule(
                nts.get("FB"), nts.get("LET")
        ));
        rules.add(new Rule(
                nts.get("FB"), nts.get("AF")
        ));
        rules.add(new Rule(
                nts.get("FB"), nts.get("GF")
        ));
        rules.add(new Rule(
                nts.get("NS"), ts.get("ns"), ts.get("id")
        ));
        rules.add(new Rule(
                nts.get("DEFN"), ts.get("defn"), ts.get("id"), ts.get("["), nts.get("AI"), ts.get("]"), nts.get("FORM")
        ));
        rules.add(new Rule(
                nts.get("LET"), ts.get("let"), ts.get("["), nts.get("BS"), ts.get("]"), nts.get("ES")
        ));

        rules.add(new Rule(
                nts.get("BS"), LL1ParserGenerator.epsilon
        ));

        rules.add(new Rule(
                nts.get("BS"), nts.get("B"), nts.get("BS")
        ));

        rules.add(new Rule(
                nts.get("B"), ts.get("id"), nts.get("FORM")
        ));
        rules.add(new Rule(
                nts.get("ES"), LL1ParserGenerator.epsilon
        ));
        rules.add(new Rule(
                nts.get("ES"), nts.get("F"), nts.get("ES")
        ));
        rules.add(new Rule(
                nts.get("AF"), nts.get("OP"), nts.get("FORMM")
        ));

        rules.add(new Rule(
                nts.get("FORMM"), LL1ParserGenerator.epsilon
        ));

        rules.add(new Rule(
                nts.get("FORMM"), nts.get("FORM"), nts.get("FORMM")
        ));

        rules.add(new Rule(
                nts.get("GF"), ts.get("id"), nts.get("FORMM")
        ));

        rules.add(new Rule(
                nts.get("AI"), LL1ParserGenerator.epsilon
        ));

        rules.add(new Rule(
                nts.get("AI"), ts.get("id"), nts.get("AI")
        ));

        rules.add(new Rule(
                nts.get("FORM"), nts.get("ATOM")
        ));

        rules.add(new Rule(
                nts.get("FORM"), nts.get("F")
        ));

        rules.add(new Rule(
                nts.get("ATOM"), ts.get("id")
        ));
        rules.add(new Rule(
                nts.get("ATOM"), ts.get("number")
        ));
        rules.add(new Rule(
                nts.get("ATOM"), ts.get("string")
        ));

        rules.add(new Rule(
                nts.get("OP"), ts.get("+")
        ));
        rules.add(new Rule(
                nts.get("OP"), ts.get("-")
        ));
        rules.add(new Rule(
                nts.get("OP"), ts.get("*")
        ));
        rules.add(new Rule(
                nts.get("OP"), ts.get("/")
        ));

    }

    public static void main(String[] args) {
        LL1ParserGenerator clojureGenerator = new LL1ParserGenerator(
                rules, simpleTokens, factoryTokens, "Integer"
        );

        System.out.println("FIRST");
        Map<NonTerminal, Set<Token>> first = clojureGenerator.first();
        printTokens(first);
        System.out.println("FOLLOW");
        Map<NonTerminal, Set<Token>> follow = clojureGenerator.follow();
        printTokens(follow);
        Map<NonTerminal, Map<Token, Rule>> ntRules = clojureGenerator.ntRules();
        printNTRules(ntRules);

        clojureGenerator.generate("Clojure");
    }

    private static void printTokens(Map<NonTerminal, Set<Token>> first) {
        for (NonTerminal nt : first.keySet()) {
            System.out.print(nt.name() + ": ");
            for (Token token : first.get(nt)) {
                System.out.print(token.name() + ", ");
            }
            System.out.println();
        }
        System.out.println();
    }

    public static void printNTRules(Map<NonTerminal, Map<Token, Rule>> ntRules) {
        for (Map.Entry<NonTerminal, Map<Token, Rule>> entry : ntRules.entrySet()) {
            NonTerminal nt = entry.getKey();
            Map<Token, Rule> ruleMap = entry.getValue();

            System.out.println(nt);
            for (Map.Entry<Token, Rule> ruleEntry : ruleMap.entrySet()) {
                Token token = ruleEntry.getKey();
                Rule rule = ruleEntry.getValue();
                System.out.println("\tToken: " + token);
                System.out.println("\tRule: " + rule);
            }
            System.out.println();
        }
    }

}

