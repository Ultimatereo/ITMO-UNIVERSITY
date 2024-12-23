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

public class ArithmeticMain {
    private final static Map<String, NonTerminal> nts = Map.of(
            "S", new NonTerminal("S"),
            "S'", new NonTerminal("SS"),
            "E", new NonTerminal("E"),
            "E'", new NonTerminal("EE"),
            "T", new NonTerminal("T"),
            "T'", new NonTerminal("TT"),
            "F", new NonTerminal("F"),
            "ADD", new NonTerminal("ADD"),
            "MUL", new NonTerminal("MUL"),
            "LOG", new NonTerminal("LOG")
    );

    private final static Map<String, Token> ts = Map.of(
            "+", new Symbol("+"),
            "*", new Symbol("*"),
            "(", new Symbol("("),
            ")", new Symbol(")"),
            "n", new Token("n"),
            "fib", new KeyWord("fib"),
            "//", new Symbol("//")
    );
    private final static List<Rule> rules = List.of(
            new Rule("t.value = EE2.value;\n",
                    nts.get("E"), nts.get("T"), nts.get("E'")),
            new Rule("t.value = EE4.value;\n",
                    nts.get("E'"), ts.get("+"), nts.get("T"), nts.get("ADD"), nts.get("E'")),
            new Rule("t.value = values[values.length - 1];\n",
                    nts.get("E'"), LL1ParserGenerator.epsilon),
            new Rule("t.value = TT2.value;\n",
                    nts.get("T"), nts.get("S"), nts.get("T'")),
            new Rule("t.value = TT4.value;\n",
                    nts.get("T'"), ts.get("*"), nts.get("S"), nts.get("MUL"), nts.get("T'")),
            new Rule("t.value = values[values.length - 1];\n",
                    nts.get("T'"), LL1ParserGenerator.epsilon),
            new Rule("t.value = Integer.parseInt(((Token) token1.node).value());\n",
                    nts.get("F"), ts.get("n")),
            new Rule("t.value = E2.value;\n",
                    nts.get("F"), ts.get("("), nts.get("E"), ts.get(")")),
            new Rule("t.value = values[values.length - 1] + values[values.length - 3];\n",
                    nts.get("ADD"), LL1ParserGenerator.epsilon),
            new Rule("t.value = values[values.length - 1] * values[values.length - 3];\n",
                    nts.get("MUL"), LL1ParserGenerator.epsilon),
            new Rule("""
                    Integer n = E3.value;
                    \t\t\t\tint fib = 1;
                    \t\t\t\tint prevFib = 1;
                                        
                    \t\t\t\tfor (int i = 2; i < n; i++) {
                    \t\t\t\t    int temp = fib;
                    \t\t\t\t    fib += prevFib;
                    \t\t\t\t    prevFib = temp;
                    \t\t\t\t}
                    \t\t\t\tt.value = fib;
                    """,
                    nts.get("F"), ts.get("fib"), ts.get("("), nts.get("E"), ts.get(")")),
            new Rule("""
                    if (value2 == null) {
                    \t\t\t\t    t.value = value1;
                    \t\t\t\t} else {
                    \t\t\t\t    t.value = value2;
                    \t\t\t\t}
                    """,
                    nts.get("S"), nts.get("F"), nts.get("S'")),
            new Rule("""
                    if (value3 == null) {
                    \t\t\t\t    t.value = (int) (Math.log(values[values.length - 1]) / Math.log(value2));
                    \t\t\t\t} else {
                    \t\t\t\t    t.value = (int) (Math.log(values[values.length - 1]) / Math.log(value3));
                    \t\t\t\t}
                    """,
                    nts.get("S'"), ts.get("//"), nts.get("F"), nts.get("S'")),
            new Rule("",
                    nts.get("S'"), LL1ParserGenerator.epsilon)
    );

    private final static List<Rule> notLL1Rules = List.of(
            new Rule(nts.get("E"), nts.get("E"), ts.get("+"), nts.get("T")),
            new Rule(nts.get("E"), nts.get("T")),
            new Rule(nts.get("T"), nts.get("T"), ts.get("*"), nts.get("F")),
            new Rule(nts.get("T"), nts.get("F")),
            new Rule(nts.get("F"), ts.get("("), nts.get("E"), ts.get(")")),
            new Rule(nts.get("F"), ts.get("n"))
    );
    private final static List<SimpleToken> simpleTokens = List.of(
            (SimpleToken) ts.get("+"),
            (SimpleToken) ts.get("*"),
            (SimpleToken) ts.get("("),
            (SimpleToken) ts.get(")"),
            (SimpleToken) ts.get("fib"),
            (SimpleToken) ts.get("//")
    );
    private final static List<TokenRegexFactory> factoryTokens = List.of(
            new TokenRegexFactory("n", "\\d+")
    );


    public static void main(String[] args) {
        LL1ParserGenerator arithmeticGenerator = new LL1ParserGenerator(
                rules, simpleTokens, factoryTokens, "Integer"
        );
        arithmeticGenerator.turnOnSemantics();

        System.out.println("FIRST");
        Map<NonTerminal, Set<Token>> first = arithmeticGenerator.first();
        printTokens(first);
        System.out.println("FOLLOW");
        Map<NonTerminal, Set<Token>> follow = arithmeticGenerator.follow();
        printTokens(follow);
        Map<NonTerminal, Map<Token, Rule>> ntRules = arithmeticGenerator.ntRules();
        printNTRules(ntRules);
        try {
            LL1ParserGenerator wrongArithmeticGenerator = new LL1ParserGenerator(notLL1Rules, simpleTokens, factoryTokens, "Integer");
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        }

        arithmeticGenerator.generate("Arithmetic");
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