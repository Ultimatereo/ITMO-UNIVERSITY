// Generated from D:/KT/MT/lab3/src/Clojure.g4 by ANTLR 4.13.1
package antlr.parser;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class ClojureParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		PLUS=1, MINUS=2, MULTIPLY=3, DIVIDE=4, GREATER_THAN_OR_EQUAL_TO=5, LESS_THAN_OR_EQUAL_TO=6, 
		GREATER_THAN=7, LESS_THAN=8, EQUAL_TO=9, LPAREN=10, RPAREN=11, LBRACKET=12, 
		RBRACKET=13, DEFN=14, NS=15, LET=16, ID=17, NUMBER=18, STRING=19, WS=20;
	public static final int
		RULE_program = 0, RULE_surface_function = 1, RULE_function = 2, RULE_function_body = 3, 
		RULE_ns = 4, RULE_defn = 5, RULE_let = 6, RULE_bindings = 7, RULE_binding = 8, 
		RULE_exprs = 9, RULE_arithmetic_function = 10, RULE_compare_function = 11, 
		RULE_general_function = 12, RULE_args_ids = 13, RULE_form = 14, RULE_atom = 15, 
		RULE_operation = 16, RULE_comparison = 17;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "surface_function", "function", "function_body", "ns", "defn", 
			"let", "bindings", "binding", "exprs", "arithmetic_function", "compare_function", 
			"general_function", "args_ids", "form", "atom", "operation", "comparison"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'+'", "'-'", "'*'", "'/'", "'>='", "'<='", "'>'", "'<'", "'='", 
			"'('", "')'", "'['", "']'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "PLUS", "MINUS", "MULTIPLY", "DIVIDE", "GREATER_THAN_OR_EQUAL_TO", 
			"LESS_THAN_OR_EQUAL_TO", "GREATER_THAN", "LESS_THAN", "EQUAL_TO", "LPAREN", 
			"RPAREN", "LBRACKET", "RBRACKET", "DEFN", "NS", "LET", "ID", "NUMBER", 
			"STRING", "WS"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "Clojure.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public ClojureParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(ClojureParser.EOF, 0); }
		public List<Surface_functionContext> surface_function() {
			return getRuleContexts(Surface_functionContext.class);
		}
		public Surface_functionContext surface_function(int i) {
			return getRuleContext(Surface_functionContext.class,i);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterProgram(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitProgram(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitProgram(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(39);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==LPAREN) {
				{
				{
				setState(36);
				surface_function();
				}
				}
				setState(41);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(42);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Surface_functionContext extends ParserRuleContext {
		public FunctionContext function() {
			return getRuleContext(FunctionContext.class,0);
		}
		public Surface_functionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_surface_function; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterSurface_function(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitSurface_function(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitSurface_function(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Surface_functionContext surface_function() throws RecognitionException {
		Surface_functionContext _localctx = new Surface_functionContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_surface_function);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(44);
			function();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FunctionContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(ClojureParser.LPAREN, 0); }
		public Function_bodyContext function_body() {
			return getRuleContext(Function_bodyContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(ClojureParser.RPAREN, 0); }
		public FunctionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_function; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterFunction(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitFunction(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitFunction(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FunctionContext function() throws RecognitionException {
		FunctionContext _localctx = new FunctionContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_function);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(46);
			match(LPAREN);
			setState(47);
			function_body();
			setState(48);
			match(RPAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Function_bodyContext extends ParserRuleContext {
		public NsContext ns() {
			return getRuleContext(NsContext.class,0);
		}
		public DefnContext defn() {
			return getRuleContext(DefnContext.class,0);
		}
		public LetContext let() {
			return getRuleContext(LetContext.class,0);
		}
		public Arithmetic_functionContext arithmetic_function() {
			return getRuleContext(Arithmetic_functionContext.class,0);
		}
		public General_functionContext general_function() {
			return getRuleContext(General_functionContext.class,0);
		}
		public Compare_functionContext compare_function() {
			return getRuleContext(Compare_functionContext.class,0);
		}
		public Function_bodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_function_body; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterFunction_body(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitFunction_body(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitFunction_body(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Function_bodyContext function_body() throws RecognitionException {
		Function_bodyContext _localctx = new Function_bodyContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_function_body);
		try {
			setState(56);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case NS:
				enterOuterAlt(_localctx, 1);
				{
				setState(50);
				ns();
				}
				break;
			case DEFN:
				enterOuterAlt(_localctx, 2);
				{
				setState(51);
				defn();
				}
				break;
			case LET:
				enterOuterAlt(_localctx, 3);
				{
				setState(52);
				let();
				}
				break;
			case PLUS:
			case MINUS:
			case MULTIPLY:
			case DIVIDE:
				enterOuterAlt(_localctx, 4);
				{
				setState(53);
				arithmetic_function();
				}
				break;
			case ID:
				enterOuterAlt(_localctx, 5);
				{
				setState(54);
				general_function();
				}
				break;
			case GREATER_THAN_OR_EQUAL_TO:
			case LESS_THAN_OR_EQUAL_TO:
			case GREATER_THAN:
			case LESS_THAN:
			case EQUAL_TO:
				enterOuterAlt(_localctx, 6);
				{
				setState(55);
				compare_function();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class NsContext extends ParserRuleContext {
		public TerminalNode NS() { return getToken(ClojureParser.NS, 0); }
		public TerminalNode ID() { return getToken(ClojureParser.ID, 0); }
		public NsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ns; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterNs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitNs(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitNs(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NsContext ns() throws RecognitionException {
		NsContext _localctx = new NsContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_ns);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(58);
			match(NS);
			setState(59);
			match(ID);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DefnContext extends ParserRuleContext {
		public TerminalNode DEFN() { return getToken(ClojureParser.DEFN, 0); }
		public TerminalNode ID() { return getToken(ClojureParser.ID, 0); }
		public TerminalNode LBRACKET() { return getToken(ClojureParser.LBRACKET, 0); }
		public Args_idsContext args_ids() {
			return getRuleContext(Args_idsContext.class,0);
		}
		public TerminalNode RBRACKET() { return getToken(ClojureParser.RBRACKET, 0); }
		public FormContext form() {
			return getRuleContext(FormContext.class,0);
		}
		public DefnContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_defn; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterDefn(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitDefn(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitDefn(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DefnContext defn() throws RecognitionException {
		DefnContext _localctx = new DefnContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_defn);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(61);
			match(DEFN);
			setState(62);
			match(ID);
			setState(63);
			match(LBRACKET);
			setState(64);
			args_ids();
			setState(65);
			match(RBRACKET);
			setState(66);
			form();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LetContext extends ParserRuleContext {
		public TerminalNode LET() { return getToken(ClojureParser.LET, 0); }
		public TerminalNode LBRACKET() { return getToken(ClojureParser.LBRACKET, 0); }
		public BindingsContext bindings() {
			return getRuleContext(BindingsContext.class,0);
		}
		public TerminalNode RBRACKET() { return getToken(ClojureParser.RBRACKET, 0); }
		public ExprsContext exprs() {
			return getRuleContext(ExprsContext.class,0);
		}
		public LetContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_let; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterLet(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitLet(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitLet(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LetContext let() throws RecognitionException {
		LetContext _localctx = new LetContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_let);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(68);
			match(LET);
			setState(69);
			match(LBRACKET);
			setState(70);
			bindings();
			setState(71);
			match(RBRACKET);
			setState(72);
			exprs();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BindingsContext extends ParserRuleContext {
		public List<BindingContext> binding() {
			return getRuleContexts(BindingContext.class);
		}
		public BindingContext binding(int i) {
			return getRuleContext(BindingContext.class,i);
		}
		public BindingsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_bindings; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterBindings(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitBindings(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitBindings(this);
			else return visitor.visitChildren(this);
		}
	}

	public final BindingsContext bindings() throws RecognitionException {
		BindingsContext _localctx = new BindingsContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_bindings);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(77);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ID) {
				{
				{
				setState(74);
				binding();
				}
				}
				setState(79);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BindingContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(ClojureParser.ID, 0); }
		public FormContext form() {
			return getRuleContext(FormContext.class,0);
		}
		public BindingContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_binding; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterBinding(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitBinding(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitBinding(this);
			else return visitor.visitChildren(this);
		}
	}

	public final BindingContext binding() throws RecognitionException {
		BindingContext _localctx = new BindingContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_binding);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(80);
			match(ID);
			setState(81);
			form();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExprsContext extends ParserRuleContext {
		public List<FunctionContext> function() {
			return getRuleContexts(FunctionContext.class);
		}
		public FunctionContext function(int i) {
			return getRuleContext(FunctionContext.class,i);
		}
		public ExprsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprs; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterExprs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitExprs(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitExprs(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExprsContext exprs() throws RecognitionException {
		ExprsContext _localctx = new ExprsContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_exprs);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(86);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==LPAREN) {
				{
				{
				setState(83);
				function();
				}
				}
				setState(88);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Arithmetic_functionContext extends ParserRuleContext {
		public OperationContext operation() {
			return getRuleContext(OperationContext.class,0);
		}
		public List<FormContext> form() {
			return getRuleContexts(FormContext.class);
		}
		public FormContext form(int i) {
			return getRuleContext(FormContext.class,i);
		}
		public Arithmetic_functionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arithmetic_function; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterArithmetic_function(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitArithmetic_function(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitArithmetic_function(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Arithmetic_functionContext arithmetic_function() throws RecognitionException {
		Arithmetic_functionContext _localctx = new Arithmetic_functionContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_arithmetic_function);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(89);
			operation();
			setState(93);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 918528L) != 0)) {
				{
				{
				setState(90);
				form();
				}
				}
				setState(95);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Compare_functionContext extends ParserRuleContext {
		public ComparisonContext comparison() {
			return getRuleContext(ComparisonContext.class,0);
		}
		public List<FormContext> form() {
			return getRuleContexts(FormContext.class);
		}
		public FormContext form(int i) {
			return getRuleContext(FormContext.class,i);
		}
		public Compare_functionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compare_function; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterCompare_function(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitCompare_function(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitCompare_function(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Compare_functionContext compare_function() throws RecognitionException {
		Compare_functionContext _localctx = new Compare_functionContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_compare_function);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(96);
			comparison();
			setState(97);
			form();
			setState(98);
			form();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class General_functionContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(ClojureParser.ID, 0); }
		public List<FormContext> form() {
			return getRuleContexts(FormContext.class);
		}
		public FormContext form(int i) {
			return getRuleContext(FormContext.class,i);
		}
		public General_functionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_general_function; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterGeneral_function(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitGeneral_function(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitGeneral_function(this);
			else return visitor.visitChildren(this);
		}
	}

	public final General_functionContext general_function() throws RecognitionException {
		General_functionContext _localctx = new General_functionContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_general_function);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(100);
			match(ID);
			setState(104);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 918528L) != 0)) {
				{
				{
				setState(101);
				form();
				}
				}
				setState(106);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Args_idsContext extends ParserRuleContext {
		public List<TerminalNode> ID() { return getTokens(ClojureParser.ID); }
		public TerminalNode ID(int i) {
			return getToken(ClojureParser.ID, i);
		}
		public Args_idsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_args_ids; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterArgs_ids(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitArgs_ids(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitArgs_ids(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Args_idsContext args_ids() throws RecognitionException {
		Args_idsContext _localctx = new Args_idsContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_args_ids);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(110);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ID) {
				{
				{
				setState(107);
				match(ID);
				}
				}
				setState(112);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FormContext extends ParserRuleContext {
		public AtomContext atom() {
			return getRuleContext(AtomContext.class,0);
		}
		public FunctionContext function() {
			return getRuleContext(FunctionContext.class,0);
		}
		public FormContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_form; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterForm(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitForm(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitForm(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FormContext form() throws RecognitionException {
		FormContext _localctx = new FormContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_form);
		try {
			setState(115);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case ID:
			case NUMBER:
			case STRING:
				enterOuterAlt(_localctx, 1);
				{
				setState(113);
				atom();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(114);
				function();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AtomContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(ClojureParser.ID, 0); }
		public TerminalNode NUMBER() { return getToken(ClojureParser.NUMBER, 0); }
		public TerminalNode STRING() { return getToken(ClojureParser.STRING, 0); }
		public AtomContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_atom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterAtom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitAtom(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitAtom(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AtomContext atom() throws RecognitionException {
		AtomContext _localctx = new AtomContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_atom);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(117);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 917504L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class OperationContext extends ParserRuleContext {
		public TerminalNode PLUS() { return getToken(ClojureParser.PLUS, 0); }
		public TerminalNode MINUS() { return getToken(ClojureParser.MINUS, 0); }
		public TerminalNode MULTIPLY() { return getToken(ClojureParser.MULTIPLY, 0); }
		public TerminalNode DIVIDE() { return getToken(ClojureParser.DIVIDE, 0); }
		public OperationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_operation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterOperation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitOperation(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitOperation(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OperationContext operation() throws RecognitionException {
		OperationContext _localctx = new OperationContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_operation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(119);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 30L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ComparisonContext extends ParserRuleContext {
		public TerminalNode GREATER_THAN_OR_EQUAL_TO() { return getToken(ClojureParser.GREATER_THAN_OR_EQUAL_TO, 0); }
		public TerminalNode LESS_THAN_OR_EQUAL_TO() { return getToken(ClojureParser.LESS_THAN_OR_EQUAL_TO, 0); }
		public TerminalNode GREATER_THAN() { return getToken(ClojureParser.GREATER_THAN, 0); }
		public TerminalNode LESS_THAN() { return getToken(ClojureParser.LESS_THAN, 0); }
		public TerminalNode EQUAL_TO() { return getToken(ClojureParser.EQUAL_TO, 0); }
		public ComparisonContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_comparison; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).enterComparison(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ClojureListener ) ((ClojureListener)listener).exitComparison(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ClojureVisitor ) return ((ClojureVisitor<? extends T>)visitor).visitComparison(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ComparisonContext comparison() throws RecognitionException {
		ComparisonContext _localctx = new ComparisonContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_comparison);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(121);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 992L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u0001\u0014|\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002"+
		"\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f"+
		"\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0001\u0000\u0005\u0000"+
		"&\b\u0000\n\u0000\f\u0000)\t\u0000\u0001\u0000\u0001\u0000\u0001\u0001"+
		"\u0001\u0001\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0003"+
		"\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0003\u0003"+
		"9\b\u0003\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0006"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0007"+
		"\u0005\u0007L\b\u0007\n\u0007\f\u0007O\t\u0007\u0001\b\u0001\b\u0001\b"+
		"\u0001\t\u0005\tU\b\t\n\t\f\tX\t\t\u0001\n\u0001\n\u0005\n\\\b\n\n\n\f"+
		"\n_\t\n\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\f\u0001"+
		"\f\u0005\fg\b\f\n\f\f\fj\t\f\u0001\r\u0005\rm\b\r\n\r\f\rp\t\r\u0001\u000e"+
		"\u0001\u000e\u0003\u000et\b\u000e\u0001\u000f\u0001\u000f\u0001\u0010"+
		"\u0001\u0010\u0001\u0011\u0001\u0011\u0001\u0011\u0000\u0000\u0012\u0000"+
		"\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c"+
		"\u001e \"\u0000\u0003\u0001\u0000\u0011\u0013\u0001\u0000\u0001\u0004"+
		"\u0001\u0000\u0005\tu\u0000\'\u0001\u0000\u0000\u0000\u0002,\u0001\u0000"+
		"\u0000\u0000\u0004.\u0001\u0000\u0000\u0000\u00068\u0001\u0000\u0000\u0000"+
		"\b:\u0001\u0000\u0000\u0000\n=\u0001\u0000\u0000\u0000\fD\u0001\u0000"+
		"\u0000\u0000\u000eM\u0001\u0000\u0000\u0000\u0010P\u0001\u0000\u0000\u0000"+
		"\u0012V\u0001\u0000\u0000\u0000\u0014Y\u0001\u0000\u0000\u0000\u0016`"+
		"\u0001\u0000\u0000\u0000\u0018d\u0001\u0000\u0000\u0000\u001an\u0001\u0000"+
		"\u0000\u0000\u001cs\u0001\u0000\u0000\u0000\u001eu\u0001\u0000\u0000\u0000"+
		" w\u0001\u0000\u0000\u0000\"y\u0001\u0000\u0000\u0000$&\u0003\u0002\u0001"+
		"\u0000%$\u0001\u0000\u0000\u0000&)\u0001\u0000\u0000\u0000\'%\u0001\u0000"+
		"\u0000\u0000\'(\u0001\u0000\u0000\u0000(*\u0001\u0000\u0000\u0000)\'\u0001"+
		"\u0000\u0000\u0000*+\u0005\u0000\u0000\u0001+\u0001\u0001\u0000\u0000"+
		"\u0000,-\u0003\u0004\u0002\u0000-\u0003\u0001\u0000\u0000\u0000./\u0005"+
		"\n\u0000\u0000/0\u0003\u0006\u0003\u000001\u0005\u000b\u0000\u00001\u0005"+
		"\u0001\u0000\u0000\u000029\u0003\b\u0004\u000039\u0003\n\u0005\u00004"+
		"9\u0003\f\u0006\u000059\u0003\u0014\n\u000069\u0003\u0018\f\u000079\u0003"+
		"\u0016\u000b\u000082\u0001\u0000\u0000\u000083\u0001\u0000\u0000\u0000"+
		"84\u0001\u0000\u0000\u000085\u0001\u0000\u0000\u000086\u0001\u0000\u0000"+
		"\u000087\u0001\u0000\u0000\u00009\u0007\u0001\u0000\u0000\u0000:;\u0005"+
		"\u000f\u0000\u0000;<\u0005\u0011\u0000\u0000<\t\u0001\u0000\u0000\u0000"+
		"=>\u0005\u000e\u0000\u0000>?\u0005\u0011\u0000\u0000?@\u0005\f\u0000\u0000"+
		"@A\u0003\u001a\r\u0000AB\u0005\r\u0000\u0000BC\u0003\u001c\u000e\u0000"+
		"C\u000b\u0001\u0000\u0000\u0000DE\u0005\u0010\u0000\u0000EF\u0005\f\u0000"+
		"\u0000FG\u0003\u000e\u0007\u0000GH\u0005\r\u0000\u0000HI\u0003\u0012\t"+
		"\u0000I\r\u0001\u0000\u0000\u0000JL\u0003\u0010\b\u0000KJ\u0001\u0000"+
		"\u0000\u0000LO\u0001\u0000\u0000\u0000MK\u0001\u0000\u0000\u0000MN\u0001"+
		"\u0000\u0000\u0000N\u000f\u0001\u0000\u0000\u0000OM\u0001\u0000\u0000"+
		"\u0000PQ\u0005\u0011\u0000\u0000QR\u0003\u001c\u000e\u0000R\u0011\u0001"+
		"\u0000\u0000\u0000SU\u0003\u0004\u0002\u0000TS\u0001\u0000\u0000\u0000"+
		"UX\u0001\u0000\u0000\u0000VT\u0001\u0000\u0000\u0000VW\u0001\u0000\u0000"+
		"\u0000W\u0013\u0001\u0000\u0000\u0000XV\u0001\u0000\u0000\u0000Y]\u0003"+
		" \u0010\u0000Z\\\u0003\u001c\u000e\u0000[Z\u0001\u0000\u0000\u0000\\_"+
		"\u0001\u0000\u0000\u0000][\u0001\u0000\u0000\u0000]^\u0001\u0000\u0000"+
		"\u0000^\u0015\u0001\u0000\u0000\u0000_]\u0001\u0000\u0000\u0000`a\u0003"+
		"\"\u0011\u0000ab\u0003\u001c\u000e\u0000bc\u0003\u001c\u000e\u0000c\u0017"+
		"\u0001\u0000\u0000\u0000dh\u0005\u0011\u0000\u0000eg\u0003\u001c\u000e"+
		"\u0000fe\u0001\u0000\u0000\u0000gj\u0001\u0000\u0000\u0000hf\u0001\u0000"+
		"\u0000\u0000hi\u0001\u0000\u0000\u0000i\u0019\u0001\u0000\u0000\u0000"+
		"jh\u0001\u0000\u0000\u0000km\u0005\u0011\u0000\u0000lk\u0001\u0000\u0000"+
		"\u0000mp\u0001\u0000\u0000\u0000nl\u0001\u0000\u0000\u0000no\u0001\u0000"+
		"\u0000\u0000o\u001b\u0001\u0000\u0000\u0000pn\u0001\u0000\u0000\u0000"+
		"qt\u0003\u001e\u000f\u0000rt\u0003\u0004\u0002\u0000sq\u0001\u0000\u0000"+
		"\u0000sr\u0001\u0000\u0000\u0000t\u001d\u0001\u0000\u0000\u0000uv\u0007"+
		"\u0000\u0000\u0000v\u001f\u0001\u0000\u0000\u0000wx\u0007\u0001\u0000"+
		"\u0000x!\u0001\u0000\u0000\u0000yz\u0007\u0002\u0000\u0000z#\u0001\u0000"+
		"\u0000\u0000\b\'8MV]hns";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}