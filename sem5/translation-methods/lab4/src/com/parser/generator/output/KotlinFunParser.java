package com.parser.generator.output;

import com.parser.generator.lexic.LexicalAnalyzer;
import com.parser.generator.lexic.factory.TokenRegexFactory;
import com.parser.generator.lexic.token.*;
import com.parser.generator.rule.NonTerminal;
import com.parser.generator.utils.Tree;

import java.io.InputStream;
import java.text.ParseException;
import java.util.List;

public class KotlinFunParser {
	private final static List<SimpleToken> simpleTokens = List.of(
		new Symbol(":", ":"),
		new KeyWord("fun"),
		new Symbol("(", "("),
		new Symbol(")", ")"),
		new Symbol(",", ",")
	);
	private final static List<TokenRegexFactory> factoryTokens = List.of(
		new TokenRegexFactory("name", "[a-zA-Z_][a-zA-Z0-9_]*")
	);
	LexicalAnalyzer lex;

	public Tree<Integer> parse(InputStream is) throws ParseException {
		lex = new LexicalAnalyzer(is, factoryTokens, simpleTokens);
		lex.nextToken();
		return S();
	}
	private Tree<Integer> S(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "FUN" -> {
				assert lex.curToken().name().equals("FUN");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> FN2 = FN();
				assert lex.curToken().name().equals("(");
				Tree<Integer> token3 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> FA4 = FA();
				assert lex.curToken().name().equals(")");
				Tree<Integer> token5 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> RT6 = RT();
				Tree<Integer> t = new Tree<>(new NonTerminal("S"), token1, FN2, token3, FA4, token5, RT6);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> FN(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "name" -> {
				assert lex.curToken().name().equals("name");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("FN"), token1);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> FA(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case ")" -> {
				Tree<Integer> t = new Tree<>(new NonTerminal("FA"));
				return t;
			}
			case "name" -> {
				assert lex.curToken().name().equals("name");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				assert lex.curToken().name().equals(":");
				Tree<Integer> token2 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> T3 = T();
				Tree<Integer> FAC4 = FAC();
				Tree<Integer> t = new Tree<>(new NonTerminal("FA"), token1, token2, T3, FAC4);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> FAC(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "," -> {
				assert lex.curToken().name().equals(",");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				assert lex.curToken().name().equals("name");
				Tree<Integer> token2 = new Tree<>(lex.curToken());
				lex.nextToken();
				assert lex.curToken().name().equals(":");
				Tree<Integer> token3 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> T4 = T();
				Tree<Integer> FAC5 = FAC();
				Tree<Integer> t = new Tree<>(new NonTerminal("FAC"), token1, token2, token3, T4, FAC5);
				return t;
			}
			case ")" -> {
				Tree<Integer> t = new Tree<>(new NonTerminal("FAC"));
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> RT(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case ":" -> {
				assert lex.curToken().name().equals(":");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> T2 = T();
				Tree<Integer> t = new Tree<>(new NonTerminal("RT"), token1, T2);
				return t;
			}
			case "$" -> {
				Tree<Integer> t = new Tree<>(new NonTerminal("RT"));
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

	private Tree<Integer> T(Integer... values) throws ParseException {
		switch (lex.curToken().name()) {
			case "name" -> {
				assert lex.curToken().name().equals("name");
				Tree<Integer> token1 = new Tree<>(lex.curToken());
				lex.nextToken();
				Tree<Integer> t = new Tree<>(new NonTerminal("T"), token1);
				return t;
			}
			default -> throw new IllegalStateException("Unexpected value: " + lex.curToken());
		}

	}

}
