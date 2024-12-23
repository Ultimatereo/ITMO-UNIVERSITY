package com.parser.generator;

import com.parser.generator.lexic.factory.TokenRegexFactory;
import com.parser.generator.lexic.token.KeyWord;
import com.parser.generator.lexic.token.SimpleToken;
import com.parser.generator.lexic.token.Symbol;
import com.parser.generator.lexic.token.Token;
import com.parser.generator.rule.NonTerminal;
import com.parser.generator.rule.Rule;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class Lab2Main {
    private final static Map<String, NonTerminal> nts = Map.of(
            "S", new NonTerminal("S"),
            "FN", new NonTerminal("FN"),
            "FA", new NonTerminal("FA"),
            "RT", new NonTerminal("RT"),
            "FAC", new NonTerminal("FAC"),
            "T", new NonTerminal("T")
    );

    private final static Map<String, Token> ts = Map.of(
            ":", new Symbol(":"),
            "fun", new KeyWord("fun"),
            "(", new Symbol("("),
            ")", new Symbol(")"),
            ",", new Symbol(","),
            "name", new Token("name")
    );
    private final static List<Rule> rules = List.of(
            new Rule(nts.get("S"), ts.get("fun"), nts.get("FN"), ts.get("("), nts.get("FA"), ts.get(")"), nts.get("RT")),
            new Rule(nts.get("FN"), ts.get("name")),
            new Rule(nts.get("FA"), LL1ParserGenerator.epsilon),
            new Rule(nts.get("FA"), ts.get("name"), ts.get(":"), nts.get("T"), nts.get("FAC")),
            new Rule(nts.get("FAC"), ts.get(","), ts.get("name"), ts.get(":"), nts.get("T"), nts.get("FAC")),
            new Rule(nts.get("FAC"), LL1ParserGenerator.epsilon),
            new Rule(nts.get("RT"), LL1ParserGenerator.epsilon),
            new Rule(nts.get("RT"), ts.get(":"), nts.get("T")),
            new Rule(nts.get("T"), ts.get("name"))
    );
    private final static List<SimpleToken> simpleTokens = List.of(
            (SimpleToken) ts.get(":"),
            (SimpleToken) ts.get("fun"),
            (SimpleToken) ts.get("("),
            (SimpleToken) ts.get(")"),
            (SimpleToken) ts.get(",")
    );
    private final static List<TokenRegexFactory> factoryTokens = List.of(
            new TokenRegexFactory("name", "[a-zA-Z_][a-zA-Z0-9_]*")
    );

    public static void main(String[] args) {
        LL1ParserGenerator kotlinGenerator = new LL1ParserGenerator(
                rules, simpleTokens, factoryTokens, "Integer"
        );

        System.out.println("FIRST");
        Map<NonTerminal, Set<Token>> first = kotlinGenerator.first();
        printTokens(first);
        System.out.println("FOLLOW");
        Map<NonTerminal, Set<Token>> follow = kotlinGenerator.follow();
        printTokens(follow);
        Map<NonTerminal, Map<Token, Rule>> ntRules = kotlinGenerator.ntRules();
        printNTRules(ntRules);

        kotlinGenerator.generate("KotlinFun");
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

