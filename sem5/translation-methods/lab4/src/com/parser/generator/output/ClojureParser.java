package com.parser.generator.output;

import com.parser.generator.lexic.LexicalAnalyzer;
import com.parser.generator.lexic.factory.TokenRegexFactory;
import com.parser.generator.lexic.token.*;
import com.parser.generator.rule.NonTerminal;
import com.parser.generator.utils.Tree;

import java.io.InputStream;
import java.text.ParseException;
import java.util.List;

public class ClojureParser {
	private final static List<SimpleToken> simpleTokens = List.of(
		new KeyWord("ns"),
		new KeyWord("defn"),
		new Symbol("(", "("),
		new Symbol(")", ")"),
		new Symbol("*", "*"),
		new Symbol("+", "+"),
		new Symbol("-", "-"),
		new Symbol("/", "/"),
		new KeyWord("let"),
		new Symbol("[", "["),
		new Symbol("]", "]")
	);
	private final static List<TokenRegexFactory> factoryTokens = List.of(
		new TokenRegexFactory("string", "\"(~[\"\\r\\n])*\""),
		new TokenRegexFactory("number", "\\d+"),
		new TokenRegexFactory("id", "[a-zA-Z][a-zA-Z0-9-]*")
	);
	LexicalAnalyzer lex;

	public Tree<Integer> parse(InputStream is) throws ParseException {
		lex = new LexicalAnalyzer(is, factoryTokens, simpleTokens);
		lex.nextToken();
		return P();
	}
	private Tree<Integer> P(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "$" -> {
				Tree<Integer> t = new Tree<>(new NonTerminal("P"));
				return t;
			}
			case "(" -> {
				Tree<Integer> SF1 = SF();
				Tree<Integer> P2 = P();
				Tree<Integer> t = new Tree<>(new NonTerminal("P"), SF1, P2);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> SF(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "(" -> {
				Tree<Integer> F1 = F();
				Tree<Integer> t = new Tree<>(new NonTerminal("SF"), F1);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> F(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "(" -> {
				assert lex.curToken().name().equals("(");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> FB2 = FB();
				assert lex.curToken().name().equals(")");
				Tree<Integer> token3 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("F"), token1, FB2, token3);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> FB(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "DEFN" -> {
				Tree<Integer> DEFN1 = DEFN();
				Tree<Integer> t = new Tree<>(new NonTerminal("FB"), DEFN1);
				return t;
			}
			case "NS" -> {
				Tree<Integer> NS1 = NS();
				Tree<Integer> t = new Tree<>(new NonTerminal("FB"), NS1);
				return t;
			}
			case "id" -> {
				Tree<Integer> GF1 = GF();
				Tree<Integer> t = new Tree<>(new NonTerminal("FB"), GF1);
				return t;
			}
			case "LET" -> {
				Tree<Integer> LET1 = LET();
				Tree<Integer> t = new Tree<>(new NonTerminal("FB"), LET1);
				return t;
			}
			case "+", "*", "/", "-" -> {
				Tree<Integer> AF1 = AF();
				Tree<Integer> t = new Tree<>(new NonTerminal("FB"), AF1);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> NS(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "NS" -> {
				assert lex.curToken().name().equals("NS");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				assert lex.curToken().name().equals("id");
				Tree<Integer> token2 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("NS"), token1, token2);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> DEFN(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "DEFN" -> {
				assert lex.curToken().name().equals("DEFN");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				assert lex.curToken().name().equals("id");
				Tree<Integer> token2 = new Tree<>(lex.curToken());
				lex.nextToken();
				assert lex.curToken().name().equals("[");
				Tree<Integer> token3 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> AI4 = AI();
				assert lex.curToken().name().equals("]");
				Tree<Integer> token5 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> FORM6 = FORM();
				Tree<Integer> t = new Tree<>(new NonTerminal("DEFN"), token1, token2, token3, AI4, token5, FORM6);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> LET(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "LET" -> {
				assert lex.curToken().name().equals("LET");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				assert lex.curToken().name().equals("[");
				Tree<Integer> token2 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> BS3 = BS();
				assert lex.curToken().name().equals("]");
				Tree<Integer> token4 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> ES5 = ES();
				Tree<Integer> t = new Tree<>(new NonTerminal("LET"), token1, token2, BS3, token4, ES5);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> BS(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "]" -> {
				Tree<Integer> t = new Tree<>(new NonTerminal("BS"));
				return t;
			}
			case "id" -> {
				Tree<Integer> B1 = B();
				Tree<Integer> BS2 = BS();
				Tree<Integer> t = new Tree<>(new NonTerminal("BS"), B1, BS2);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> B(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "id" -> {
				assert lex.curToken().name().equals("id");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> FORM2 = FORM();
				Tree<Integer> t = new Tree<>(new NonTerminal("B"), token1, FORM2);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> ES(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "(" -> {
				Tree<Integer> F1 = F();
				Tree<Integer> ES2 = ES();
				Tree<Integer> t = new Tree<>(new NonTerminal("ES"), F1, ES2);
				return t;
			}
			case ")" -> {
				Tree<Integer> t = new Tree<>(new NonTerminal("ES"));
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> AF(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "+", "*", "/", "-" -> {
				Tree<Integer> OP1 = OP();
				Tree<Integer> FORMM2 = FORMM();
				Tree<Integer> t = new Tree<>(new NonTerminal("AF"), OP1, FORMM2);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> FORMM(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "(", "id", "number", "string" -> {
				Tree<Integer> FORM1 = FORM();
				Tree<Integer> FORMM2 = FORMM();
				Tree<Integer> t = new Tree<>(new NonTerminal("FORMM"), FORM1, FORMM2);
				return t;
			}
			case ")" -> {
				Tree<Integer> t = new Tree<>(new NonTerminal("FORMM"));
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> GF(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "id" -> {
				assert lex.curToken().name().equals("id");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> FORMM2 = FORMM();
				Tree<Integer> t = new Tree<>(new NonTerminal("GF"), token1, FORMM2);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> AI(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "]" -> {
				Tree<Integer> t = new Tree<>(new NonTerminal("AI"));
				return t;
			}
			case "id" -> {
				assert lex.curToken().name().equals("id");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> AI2 = AI();
				Tree<Integer> t = new Tree<>(new NonTerminal("AI"), token1, AI2);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> FORM(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "id", "number", "string" -> {
				Tree<Integer> ATOM1 = ATOM();
				Tree<Integer> t = new Tree<>(new NonTerminal("FORM"), ATOM1);
				return t;
			}
			case "(" -> {
				Tree<Integer> F1 = F();
				Tree<Integer> t = new Tree<>(new NonTerminal("FORM"), F1);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> ATOM(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "number" -> {
				assert lex.curToken().name().equals("number");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("ATOM"), token1);
				return t;
			}
			case "id" -> {
				assert lex.curToken().name().equals("id");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("ATOM"), token1);
				return t;
			}
			case "string" -> {
				assert lex.curToken().name().equals("string");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("ATOM"), token1);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> OP(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "/" -> {
				assert lex.curToken().name().equals("/");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("OP"), token1);
				return t;
			}
			case "+" -> {
				assert lex.curToken().name().equals("+");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("OP"), token1);
				return t;
			}
			case "-" -> {
				assert lex.curToken().name().equals("-");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("OP"), token1);
				return t;
			}
			case "*" -> {
				assert lex.curToken().name().equals("*");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("OP"), token1);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

}
