package com.parser.generator;

import com.parser.generator.lexic.factory.TokenRegexFactory;
import com.parser.generator.lexic.token.KeyWord;
import com.parser.generator.lexic.token.SimpleToken;
import com.parser.generator.lexic.token.Symbol;
import com.parser.generator.lexic.token.Token;
import com.parser.generator.rule.Element;
import com.parser.generator.rule.NonTerminal;
import com.parser.generator.rule.Rule;

import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

public class LL1ParserGenerator implements ParserGenerator {
    public static final Token epsilon = new Token("ε", "");
    private final static String dirPath = "D:\\KT\\MT\\lab4\\src\\com\\parser\\generator\\output\\";
    private static final String imports = """
            package com.parser.generator.output;
                        
            import com.parser.generator.lexic.LexicalAnalyzer;
            import com.parser.generator.lexic.factory.TokenRegexFactory;
            import com.parser.generator.lexic.token.*;
            import com.parser.generator.rule.NonTerminal;
            import com.parser.generator.utils.Tree;
                        
            import java.io.InputStream;
            import java.text.ParseException;
            import java.util.List;
                        
            """;
    private static final Token end = new Token("$", "");
    private final List<Rule> rules;
    private final List<SimpleToken> simpleTokens;
    private final List<TokenRegexFactory> factoryTokens;
    private final String typeRes;
    private final Map<NonTerminal, Set<Token>> first = new HashMap<>();
    private final Map<NonTerminal, Set<Token>> follow = new HashMap<>();
    private final Map<NonTerminal, Map<Token, Rule>> ntTokenRules = new HashMap<>();
    private final Map<NonTerminal, Map<Rule, List<Token>>> ntRuleTokens = new HashMap<>();
    private boolean withSemantics = false;

    public void turnOnSemantics() {
        withSemantics = true;
    }

    public LL1ParserGenerator(List<Rule> rules, List<SimpleToken> simpleTokens, List<TokenRegexFactory> factoryTokens, String typeRes) {
        this.rules = rules;
        this.simpleTokens = simpleTokens;
        this.factoryTokens = factoryTokens;
        this.typeRes = typeRes;
        createFirst();
        createFollow();
        createNTRules();
    }

    private static void deleteCommaIfNeeded(StringBuilder code) {
        if (code.length() > 1 && code.charAt(code.length() - 2) == ',') {
            code.deleteCharAt(code.length() - 2);
        }
    }

    public Map<NonTerminal, Set<Token>> first() {
        return first;
    }

    public Map<NonTerminal, Set<Token>> follow() {
        return follow;
    }

    public Map<NonTerminal, Map<Token, Rule>> ntRules() {
        return ntTokenRules;
    }

    private void createFollow() {
        NonTerminal S = rules.getFirst().leftPart();
        follow.put(S, new HashSet<>());
        follow.get(S).add(end);
        boolean change = true;
        while (change) {
            change = false;
            for (Rule rule : rules) {
                for (int i = 0; i < rule.rightPart().size(); i++) {
                    if (rule.rightPart().get(i) instanceof NonTerminal B) {
                        follow.putIfAbsent(B, new HashSet<>());
                        int oldSize = follow.get(B).size();
                        List<Element> afterB = rule.rightPart().subList(i + 1, rule.rightPart().size());

                        Set<Token> s = createFirst(afterB);
                        if (s.contains(epsilon)) {
                            s.remove(epsilon);
                            s.addAll(follow.getOrDefault(rule.leftPart(), new HashSet<>()));
                        }
                        follow.get(B).addAll(s);
                        int newSize = follow.get(B).size();
                        if (newSize > oldSize) {
                            change = true;
                        }
                    }
                }
            }
        }
    }

    private void createFirst() {
        boolean change = true;
        while (change) {
            change = false;
            for (Rule rule : rules) {
                NonTerminal nt = rule.leftPart();
                first.putIfAbsent(nt, new HashSet<>());
                int oldSize = first.get(nt).size();
                Set<Token> newTokens = createFirst(rule.rightPart());
                first.get(nt).addAll(newTokens);
                int newSize = first.get(nt).size();
                if (newSize > oldSize) {
                    change = true;
                }
            }
        }

    }

    private Set<Token> createFirst(List<Element> elements) {
        Set<Token> s = new HashSet<>();
        if (elements.isEmpty()) {
            s.add(epsilon);
            return s;
        }
        Element f = elements.getFirst();
        if (f instanceof Token) {
            s.add((Token) f);
            return s;
        }
        s = new HashSet<>(first.getOrDefault((NonTerminal) f, new HashSet<>()));
        if (s.contains(epsilon)) {
            if (elements.size() > 1) {
                s.remove(epsilon);
                s.addAll(createFirst(elements.subList(1, elements.size())));
            }
        }
        return s;
    }

    @Override
    public void generate(String generatorName) {
        StringBuilder code = new StringBuilder(imports);
        generateClass(code, generatorName);
        String fileName = dirPath + generatorName + "Parser.java";
        try (FileWriter writer = new FileWriter(fileName)) {
            writer.write(code.toString());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void generateClass(StringBuilder code, String generatorName) {
        code.append("public class ").append(generatorName).append("Parser {\n");
        generateSimpleTokens(code);
        generateFactoryTokens(code);
        code.append("\tLexicalAnalyzer lex;\n\n");
        generateCombineFunction(code, typeRes);
        generateParseFunction(code);
        Set<NonTerminal> ntWasBefore = new HashSet<>();
        for (Rule rule : rules) {
            NonTerminal nt = rule.leftPart();
            if (!ntWasBefore.contains(nt)) {
                ntWasBefore.add(nt);
                generateFunction(code, nt);
            }
        }
        code.append("}\n");
    }

    private void generateCombineFunction(StringBuilder code, String typeRes) {
        code.append("\tpublic static ").append(typeRes).append("[] combineArrays(").
                append(typeRes).append("[] array1, ").append(typeRes).append("[] array2) {\n");
        code.append("\t\t").append(typeRes).append("[] combinedArray = new ").append(typeRes)
                .append("[array1.length + array2.length];\n");
        code.append("""
                \t\tSystem.arraycopy(array1, 0, combinedArray, 0, array1.length);
                \t\tSystem.arraycopy(array2, 0, combinedArray, array1.length, array2.length);
                \t\treturn combinedArray;
                \t}
                """);
    }

    private void generateFunction(StringBuilder code, NonTerminal nt) {
        code.append("\tprivate Tree<").append(typeRes).append("> ").append(nt.name()).append("(").append(typeRes).append("... values) throws ParseException {\n");
        code.append("\t\tswitch (lex.curToken().name()) {\n");
        for (Rule rule : ntRuleTokens.get(nt).keySet()) {
            code.append("\t\t\tcase ").append(generateTokenList(ntRuleTokens.get(nt).get(rule))).append(" -> {\n");
            generateRule(code, rule);
            code.append("\t\t\t}\n");
        }
        code.append("\t\t\tdefault -> throw new IllegalStateException(\"Unexpected value: \" + lex.curToken());\n");
        code.append("\t\t}\n\n");
        code.append("\t}\n\n");
    }

    private StringBuilder generateTokenList(List<Token> tokens) {
        StringBuilder sb = new StringBuilder();
        assert !tokens.isEmpty();
        sb.append("\"").append(tokens.get(0).name()).append("\"");
        for (int i = 1; i < tokens.size(); i++) {
            sb.append(", \"").append(tokens.get(i).name()).append("\"");
        }
        return sb;
    }

    private void generateRule(StringBuilder code, Rule rule) {
        if (rule.rightPart().getFirst().equals(epsilon)) {
            String rc = "Tree<" + typeRes + "> t = new Tree<>(new NonTerminal(\"" + rule.leftPart().name() + "\"));\n";
            String rc1 = "return t;\n";
            code.append("\t\t\t\t").append(rc);
            if (withSemantics) {
                code.append("\t\t\t\t").append(rule.semantics());
            }
            code.append("\t\t\t\t").append(rc1);
            return;
        }
        List<String> commands = new ArrayList<>();
        List<String> nodes = new ArrayList<>();
        int counter = 0;
        for (Element element : rule.rightPart()) {
            counter++;
            if (element instanceof NonTerminal) {
                String var = element.name() + counter;
                nodes.add(var);
                if (withSemantics) {
                    String command = "Tree<" + typeRes + "> " + var + " = " + element.name() + "(" + getListVar(counter) + ");\n";
                    String commandVal = typeRes + " value" + counter + " = " + var + ".value;\n";
                    commands.add(command);
                    commands.add(commandVal);
                } else {
                    String command = "Tree<" + typeRes + "> " + var + " = " + element.name() + "();\n";
                    commands.add(command);
                }
            } else {
                Token token = (Token) element;
                String var = "token" + counter;
                nodes.add(var);
                String c1 = "assert lex.curToken().name().equals(\"" + element.name() + "\");\n";
                String c2 = "Tree<" + typeRes + "> " + var + " = " + "new Tree<>(lex.curToken());\n";
                String c3 = "lex.nextToken();\n";
                commands.add(c1);
                commands.add(c2);
                if (withSemantics) {
                    commands.add(typeRes + " value" + counter + " = " + var + ".value;\n");
                }
                commands.add(c3);
            }
        }
        String rc = "Tree<" + typeRes + "> t = new Tree<>(new NonTerminal(\"" + rule.leftPart().name() + "\")" + getNodes(nodes) + ");\n";
        String rc1 = "return t;\n";
        commands.add(rc);
        if (withSemantics) {
            commands.add(rule.semantics());
        }
        commands.add(rc1);
        for (String c : commands) {
            code.append("\t\t\t\t").append(c);
        }
    }

    private String getListVar(int counter) {
        StringBuilder sb = new StringBuilder();
        sb.append("combineArrays(values, new Integer[]{");
        if (counter > 1) {
            sb.append("value1");
        }
        for (int i = 2; i < counter; i++) {
            sb.append(", value").append(i);
        }
        sb.append("})");
        return sb.toString();
    }

    private String getNodes(List<String> nodes) {
        StringBuilder sb = new StringBuilder();
        for (String node : nodes) {
            sb.append(", ").append(node);
        }
        return sb.toString();
    }

    private void generateParseFunction(StringBuilder code) {
        code.append("\tpublic Tree<").append(typeRes).append("> parse(InputStream is) throws ParseException {\n");
        code.append("\t\tlex = new LexicalAnalyzer(is, factoryTokens, simpleTokens);\n");
        code.append("\t\tlex.nextToken();\n");
        code.append("\t\treturn ").append(rules.get(0).leftPart().name()).append("();\n");
        code.append("\t}\n");
    }

    private void generateFactoryTokens(StringBuilder code) {
        code.append("\tprivate final static List<TokenRegexFactory> factoryTokens = List.of(\n");
        for (TokenRegexFactory tokenRegexFactory : factoryTokens) {
            code.append("\t\tnew TokenRegexFactory(\"").append(tokenRegexFactory.name()).append("\", \"")
                    .append(doubleDashes(tokenRegexFactory.regex())).append("\"),\n");
        }
        deleteCommaIfNeeded(code);
        code.append("\t);\n");
    }

    private StringBuilder doubleDashes(String regex) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < regex.length(); i++) {
            char c = regex.charAt(i);
            if (c == '\\' || c == '"') {
                sb.append("\\").append(c);
            } else {
                sb.append(c);
            }
        }
        return sb;
    }

    private void generateSimpleTokens(StringBuilder code) {
        code.append("\tprivate final static List<SimpleToken> simpleTokens = List.of(\n");
        for (SimpleToken token : simpleTokens) {
            if (token instanceof Symbol) {
                code.append("\t\tnew Symbol(\"").append(token.name()).append("\", \"")
                        .append(token.value()).append("\"),\n");
            } else if (token instanceof KeyWord) {
                code.append("\t\tnew KeyWord(\"").append(token.value()).append("\"),\n");
            } else {
                throw new IllegalArgumentException("Unknown subclass of SimpleToken! " + token.getClass());
            }
        }
        deleteCommaIfNeeded(code);
        code.append("\t);\n");
    }

    private void createNTRules() {
        for (Rule rule : rules) {
            Set<Token> s = new HashSet<>(createFirst(rule.rightPart()));
            if (s.contains(epsilon)) {
                s.remove(epsilon);
                s.addAll(follow.get(rule.leftPart()));
            }
            for (Token token : s) {
                ntTokenRules.putIfAbsent(rule.leftPart(), new HashMap<>());
                if (ntTokenRules.get(rule.leftPart()).containsKey(token)) {
                    throw new IllegalArgumentException(
                            "There are two rules starting with token '" + token.name() + "': " +
                                    ntTokenRules.get(rule.leftPart()).get(token).toString() + " and " +
                                    rule
                    );
                }
                ntTokenRules.get(rule.leftPart()).put(token, rule);
                ntRuleTokens.putIfAbsent(rule.leftPart(), new HashMap<>());
                ntRuleTokens.get(rule.leftPart()).putIfAbsent(rule, new ArrayList<>());
                ntRuleTokens.get(rule.leftPart()).get(rule).add(token);
            }
        }
    }
}
