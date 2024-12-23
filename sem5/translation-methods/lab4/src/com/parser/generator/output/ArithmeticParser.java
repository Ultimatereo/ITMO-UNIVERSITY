package com.parser.generator.output;

import com.parser.generator.lexic.LexicalAnalyzer;
import com.parser.generator.lexic.factory.TokenRegexFactory;
import com.parser.generator.lexic.token.KeyWord;
import com.parser.generator.lexic.token.SimpleToken;
import com.parser.generator.lexic.token.Symbol;
import com.parser.generator.lexic.token.Token;
import com.parser.generator.rule.NonTerminal;
import com.parser.generator.utils.Tree;

import java.io.InputStream;
import java.text.ParseException;
import java.util.List;

public class ArithmeticParser {
    private final static List<SimpleToken> simpleTokens = List.of(
            new Symbol("+", "+"),
            new Symbol("*", "*"),
            new Symbol("(", "("),
            new Symbol(")", ")"),
            new KeyWord("fib"),
            new Symbol("//", "//")
    );
    private final static List<TokenRegexFactory> factoryTokens = List.of(
            new TokenRegexFactory("n", "\\d+")
    );
    LexicalAnalyzer lex;

    public static Integer[] combineArrays(Integer[] array1, Integer[] array2) {
        Integer[] combinedArray = new Integer[array1.length + array2.length];
        System.arraycopy(array1, 0, combinedArray, 0, array1.length);
        System.arraycopy(array2, 0, combinedArray, array1.length, array2.length);
        return combinedArray;
    }

    public Tree<Integer> parse(InputStream is) throws ParseException {
        lex = new LexicalAnalyzer(is, factoryTokens, simpleTokens);
        lex.nextToken();
        return E();
    }

    private Tree<Integer> E(Integer... values) throws ParseException {
        switch (lex.curToken().name()) {
            case "n", "FIB", "(" -> {
                Tree<Integer> T1 = T(combineArrays(values, new Integer[]{}));
                Integer value1 = T1.value;
                Tree<Integer> EE2 = EE(combineArrays(values, new Integer[]{value1}));
                Integer value2 = EE2.value;
                Tree<Integer> t = new Tree<>(new NonTerminal("E"), T1, EE2);
                t.value = EE2.value;
                return t;
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }

    }

    private Tree<Integer> EE(Integer... values) throws ParseException {
        switch (lex.curToken().name()) {
            case "+" -> {
                assert lex.curToken().name().equals("+");
                Tree<Integer> token1 = new Tree<>(lex.curToken());
                Integer value1 = token1.value;
                lex.nextToken();
                Tree<Integer> T2 = T(combineArrays(values, new Integer[]{value1}));
                Integer value2 = T2.value;
                Tree<Integer> ADD3 = ADD(combineArrays(values, new Integer[]{value1, value2}));
                Integer value3 = ADD3.value;
                Tree<Integer> EE4 = EE(combineArrays(values, new Integer[]{value1, value2, value3}));
                Integer value4 = EE4.value;
                Tree<Integer> t = new Tree<>(new NonTerminal("EE"), token1, T2, ADD3, EE4);
                t.value = EE4.value;
                return t;
            }
            case ")", "$" -> {
                Tree<Integer> t = new Tree<>(new NonTerminal("EE"));
                t.value = values[values.length - 1];
                return t;
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }

    }

    private Tree<Integer> T(Integer... values) throws ParseException {
        switch (lex.curToken().name()) {
            case "n", "FIB", "(" -> {
                Tree<Integer> S1 = S(combineArrays(values, new Integer[]{}));
                Integer value1 = S1.value;
                Tree<Integer> TT2 = TT(combineArrays(values, new Integer[]{value1}));
                Integer value2 = TT2.value;
                Tree<Integer> t = new Tree<>(new NonTerminal("T"), S1, TT2);
                t.value = TT2.value;
                return t;
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }

    }

    private Tree<Integer> TT(Integer... values) throws ParseException {
        switch (lex.curToken().name()) {
            case ")", "+", "$" -> {
                Tree<Integer> t = new Tree<>(new NonTerminal("TT"));
                t.value = values[values.length - 1];
                return t;
            }
            case "*" -> {
                assert lex.curToken().name().equals("*");
                Tree<Integer> token1 = new Tree<>(lex.curToken());
                Integer value1 = token1.value;
                lex.nextToken();
                Tree<Integer> S2 = S(combineArrays(values, new Integer[]{value1}));
                Integer value2 = S2.value;
                Tree<Integer> MUL3 = MUL(combineArrays(values, new Integer[]{value1, value2}));
                Integer value3 = MUL3.value;
                Tree<Integer> TT4 = TT(combineArrays(values, new Integer[]{value1, value2, value3}));
                Integer value4 = TT4.value;
                Tree<Integer> t = new Tree<>(new NonTerminal("TT"), token1, S2, MUL3, TT4);
                t.value = TT4.value;
                return t;
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }

    }

    private Tree<Integer> F(Integer... values) throws ParseException {
        switch (lex.curToken().name()) {
            case "n" -> {
                assert lex.curToken().name().equals("n");
                Tree<Integer> token1 = new Tree<>(lex.curToken());
                Integer value1 = token1.value;
                lex.nextToken();
                Tree<Integer> t = new Tree<>(new NonTerminal("F"), token1);
                t.value = Integer.parseInt(((Token) token1.node).value());
                return t;
            }
            case "(" -> {
                assert lex.curToken().name().equals("(");
                Tree<Integer> token1 = new Tree<>(lex.curToken());
                Integer value1 = token1.value;
                lex.nextToken();
                Tree<Integer> E2 = E(combineArrays(values, new Integer[]{value1}));
                Integer value2 = E2.value;
                assert lex.curToken().name().equals(")");
                Tree<Integer> token3 = new Tree<>(lex.curToken());
                Integer value3 = token3.value;
                lex.nextToken();
                Tree<Integer> t = new Tree<>(new NonTerminal("F"), token1, E2, token3);
                t.value = E2.value;
                return t;
            }
            case "FIB" -> {
                assert lex.curToken().name().equals("FIB");
                Tree<Integer> token1 = new Tree<>(lex.curToken());
                Integer value1 = token1.value;
                lex.nextToken();
                assert lex.curToken().name().equals("(");
                Tree<Integer> token2 = new Tree<>(lex.curToken());
                Integer value2 = token2.value;
                lex.nextToken();
                Tree<Integer> E3 = E(combineArrays(values, new Integer[]{value1, value2}));
                Integer value3 = E3.value;
                assert lex.curToken().name().equals(")");
                Tree<Integer> token4 = new Tree<>(lex.curToken());
                Integer value4 = token4.value;
                lex.nextToken();
                Tree<Integer> t = new Tree<>(new NonTerminal("F"), token1, token2, E3, token4);
                Integer n = E3.value;
                int fib = 1;
                int prevFib = 1;

                for (int i = 2; i < n; i++) {
                    int temp = fib;
                    fib += prevFib;
                    prevFib = temp;
                }
                t.value = fib;
                return t;
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }

    }

    private Tree<Integer> ADD(Integer... values) throws ParseException {
        switch (lex.curToken().name()) {
            case ")", "+", "$" -> {
                Tree<Integer> t = new Tree<>(new NonTerminal("ADD"));
                t.value = values[values.length - 1] + values[values.length - 3];
                return t;
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }

    }

    private Tree<Integer> MUL(Integer... values) throws ParseException {
        switch (lex.curToken().name()) {
            case ")", "+", "$", "*" -> {
                Tree<Integer> t = new Tree<>(new NonTerminal("MUL"));
                t.value = values[values.length - 1] * values[values.length - 3];
                return t;
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }

    }

    private Tree<Integer> S(Integer... values) throws ParseException {
        switch (lex.curToken().name()) {
            case "n", "FIB", "(" -> {
                Tree<Integer> F1 = F(combineArrays(values, new Integer[]{}));
                Integer value1 = F1.value;
                Tree<Integer> SS2 = SS(combineArrays(values, new Integer[]{value1}));
                Integer value2 = SS2.value;
                Tree<Integer> t = new Tree<>(new NonTerminal("S"), F1, SS2);
                if (value2 == null) {
                    t.value = value1;
                } else {
                    t.value = value2;
                }
                return t;
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }

    }

    private Tree<Integer> SS(Integer... values) throws ParseException {
        switch (lex.curToken().name()) {
            case "//" -> {
                assert lex.curToken().name().equals("//");
                Tree<Integer> token1 = new Tree<>(lex.curToken());
                Integer value1 = token1.value;
                lex.nextToken();
                Tree<Integer> F2 = F(combineArrays(values, new Integer[]{value1}));
                Integer value2 = F2.value;
                Tree<Integer> SS3 = SS(combineArrays(values, new Integer[]{value1, value2}));
                Integer value3 = SS3.value;
                Tree<Integer> t = new Tree<>(new NonTerminal("SS"), token1, F2, SS3);
                if (value3 == null) {
                    t.value = (int) (Math.log(values[values.length - 1]) / Math.log(value2));
                } else {
                    t.value = (int) (Math.log(values[values.length - 1]) / Math.log(value3));
                }
                return t;
            }
            case ")", "+", "$", "*" -> {
                Tree<Integer> t = new Tree<>(new NonTerminal("SS"));
                return t;
            }
            default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
        }

    }

}
