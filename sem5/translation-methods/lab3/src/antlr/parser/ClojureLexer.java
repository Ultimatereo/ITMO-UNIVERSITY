// Generated from D:/KT/MT/lab3/src/Clojure.g4 by ANTLR 4.13.1
package antlr.parser;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class ClojureLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		PLUS=1, MINUS=2, MULTIPLY=3, DIVIDE=4, GREATER_THAN_OR_EQUAL_TO=5, LESS_THAN_OR_EQUAL_TO=6, 
		GREATER_THAN=7, LESS_THAN=8, EQUAL_TO=9, LPAREN=10, RPAREN=11, LBRACKET=12, 
		RBRACKET=13, DEFN=14, NS=15, LET=16, ID=17, NUMBER=18, STRING=19, WS=20;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"PLUS", "MINUS", "MULTIPLY", "DIVIDE", "GREATER_THAN_OR_EQUAL_TO", "LESS_THAN_OR_EQUAL_TO", 
			"GREATER_THAN", "LESS_THAN", "EQUAL_TO", "LPAREN", "RPAREN", "LBRACKET", 
			"RBRACKET", "DEFN", "NS", "LET", "ID", "NUMBER", "STRING", "WS"
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


	public ClojureLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Clojure.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\u0004\u0000\u0014}\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002\u0001"+
		"\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004"+
		"\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007"+
		"\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b"+
		"\u0007\u000b\u0002\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002"+
		"\u000f\u0007\u000f\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002"+
		"\u0012\u0007\u0012\u0002\u0013\u0007\u0013\u0001\u0000\u0001\u0000\u0001"+
		"\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001"+
		"\u0004\u0001\u0004\u0001\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0006\u0001\u0006\u0001\u0007\u0001\u0007\u0001\b\u0001\b\u0001\t\u0001"+
		"\t\u0001\n\u0001\n\u0001\u000b\u0001\u000b\u0001\f\u0001\f\u0001\r\u0001"+
		"\r\u0001\r\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0001\u000e\u0001\u000f"+
		"\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u0010\u0001\u0010\u0005\u0010"+
		"T\b\u0010\n\u0010\f\u0010W\t\u0010\u0001\u0011\u0003\u0011Z\b\u0011\u0001"+
		"\u0011\u0001\u0011\u0001\u0011\u0005\u0011_\b\u0011\n\u0011\f\u0011b\t"+
		"\u0011\u0003\u0011d\b\u0011\u0001\u0011\u0001\u0011\u0004\u0011h\b\u0011"+
		"\u000b\u0011\f\u0011i\u0003\u0011l\b\u0011\u0001\u0012\u0001\u0012\u0005"+
		"\u0012p\b\u0012\n\u0012\f\u0012s\t\u0012\u0001\u0012\u0001\u0012\u0001"+
		"\u0013\u0004\u0013x\b\u0013\u000b\u0013\f\u0013y\u0001\u0013\u0001\u0013"+
		"\u0000\u0000\u0014\u0001\u0001\u0003\u0002\u0005\u0003\u0007\u0004\t\u0005"+
		"\u000b\u0006\r\u0007\u000f\b\u0011\t\u0013\n\u0015\u000b\u0017\f\u0019"+
		"\r\u001b\u000e\u001d\u000f\u001f\u0010!\u0011#\u0012%\u0013\'\u0014\u0001"+
		"\u0000\u0006\u0002\u0000AZaz\u0004\u0000--09AZaz\u0001\u000019\u0001\u0000"+
		"09\u0003\u0000\n\n\r\r\"\"\u0003\u0000\t\n\r\r  \u0084\u0000\u0001\u0001"+
		"\u0000\u0000\u0000\u0000\u0003\u0001\u0000\u0000\u0000\u0000\u0005\u0001"+
		"\u0000\u0000\u0000\u0000\u0007\u0001\u0000\u0000\u0000\u0000\t\u0001\u0000"+
		"\u0000\u0000\u0000\u000b\u0001\u0000\u0000\u0000\u0000\r\u0001\u0000\u0000"+
		"\u0000\u0000\u000f\u0001\u0000\u0000\u0000\u0000\u0011\u0001\u0000\u0000"+
		"\u0000\u0000\u0013\u0001\u0000\u0000\u0000\u0000\u0015\u0001\u0000\u0000"+
		"\u0000\u0000\u0017\u0001\u0000\u0000\u0000\u0000\u0019\u0001\u0000\u0000"+
		"\u0000\u0000\u001b\u0001\u0000\u0000\u0000\u0000\u001d\u0001\u0000\u0000"+
		"\u0000\u0000\u001f\u0001\u0000\u0000\u0000\u0000!\u0001\u0000\u0000\u0000"+
		"\u0000#\u0001\u0000\u0000\u0000\u0000%\u0001\u0000\u0000\u0000\u0000\'"+
		"\u0001\u0000\u0000\u0000\u0001)\u0001\u0000\u0000\u0000\u0003+\u0001\u0000"+
		"\u0000\u0000\u0005-\u0001\u0000\u0000\u0000\u0007/\u0001\u0000\u0000\u0000"+
		"\t1\u0001\u0000\u0000\u0000\u000b4\u0001\u0000\u0000\u0000\r7\u0001\u0000"+
		"\u0000\u0000\u000f9\u0001\u0000\u0000\u0000\u0011;\u0001\u0000\u0000\u0000"+
		"\u0013=\u0001\u0000\u0000\u0000\u0015?\u0001\u0000\u0000\u0000\u0017A"+
		"\u0001\u0000\u0000\u0000\u0019C\u0001\u0000\u0000\u0000\u001bE\u0001\u0000"+
		"\u0000\u0000\u001dJ\u0001\u0000\u0000\u0000\u001fM\u0001\u0000\u0000\u0000"+
		"!Q\u0001\u0000\u0000\u0000#Y\u0001\u0000\u0000\u0000%m\u0001\u0000\u0000"+
		"\u0000\'w\u0001\u0000\u0000\u0000)*\u0005+\u0000\u0000*\u0002\u0001\u0000"+
		"\u0000\u0000+,\u0005-\u0000\u0000,\u0004\u0001\u0000\u0000\u0000-.\u0005"+
		"*\u0000\u0000.\u0006\u0001\u0000\u0000\u0000/0\u0005/\u0000\u00000\b\u0001"+
		"\u0000\u0000\u000012\u0005>\u0000\u000023\u0005=\u0000\u00003\n\u0001"+
		"\u0000\u0000\u000045\u0005<\u0000\u000056\u0005=\u0000\u00006\f\u0001"+
		"\u0000\u0000\u000078\u0005>\u0000\u00008\u000e\u0001\u0000\u0000\u0000"+
		"9:\u0005<\u0000\u0000:\u0010\u0001\u0000\u0000\u0000;<\u0005=\u0000\u0000"+
		"<\u0012\u0001\u0000\u0000\u0000=>\u0005(\u0000\u0000>\u0014\u0001\u0000"+
		"\u0000\u0000?@\u0005)\u0000\u0000@\u0016\u0001\u0000\u0000\u0000AB\u0005"+
		"[\u0000\u0000B\u0018\u0001\u0000\u0000\u0000CD\u0005]\u0000\u0000D\u001a"+
		"\u0001\u0000\u0000\u0000EF\u0005d\u0000\u0000FG\u0005e\u0000\u0000GH\u0005"+
		"f\u0000\u0000HI\u0005n\u0000\u0000I\u001c\u0001\u0000\u0000\u0000JK\u0005"+
		"n\u0000\u0000KL\u0005s\u0000\u0000L\u001e\u0001\u0000\u0000\u0000MN\u0005"+
		"l\u0000\u0000NO\u0005e\u0000\u0000OP\u0005t\u0000\u0000P \u0001\u0000"+
		"\u0000\u0000QU\u0007\u0000\u0000\u0000RT\u0007\u0001\u0000\u0000SR\u0001"+
		"\u0000\u0000\u0000TW\u0001\u0000\u0000\u0000US\u0001\u0000\u0000\u0000"+
		"UV\u0001\u0000\u0000\u0000V\"\u0001\u0000\u0000\u0000WU\u0001\u0000\u0000"+
		"\u0000XZ\u0005-\u0000\u0000YX\u0001\u0000\u0000\u0000YZ\u0001\u0000\u0000"+
		"\u0000Zc\u0001\u0000\u0000\u0000[d\u00050\u0000\u0000\\`\u0007\u0002\u0000"+
		"\u0000]_\u0007\u0003\u0000\u0000^]\u0001\u0000\u0000\u0000_b\u0001\u0000"+
		"\u0000\u0000`^\u0001\u0000\u0000\u0000`a\u0001\u0000\u0000\u0000ad\u0001"+
		"\u0000\u0000\u0000b`\u0001\u0000\u0000\u0000c[\u0001\u0000\u0000\u0000"+
		"c\\\u0001\u0000\u0000\u0000dk\u0001\u0000\u0000\u0000eg\u0005.\u0000\u0000"+
		"fh\u0007\u0003\u0000\u0000gf\u0001\u0000\u0000\u0000hi\u0001\u0000\u0000"+
		"\u0000ig\u0001\u0000\u0000\u0000ij\u0001\u0000\u0000\u0000jl\u0001\u0000"+
		"\u0000\u0000ke\u0001\u0000\u0000\u0000kl\u0001\u0000\u0000\u0000l$\u0001"+
		"\u0000\u0000\u0000mq\u0005\"\u0000\u0000np\b\u0004\u0000\u0000on\u0001"+
		"\u0000\u0000\u0000ps\u0001\u0000\u0000\u0000qo\u0001\u0000\u0000\u0000"+
		"qr\u0001\u0000\u0000\u0000rt\u0001\u0000\u0000\u0000sq\u0001\u0000\u0000"+
		"\u0000tu\u0005\"\u0000\u0000u&\u0001\u0000\u0000\u0000vx\u0007\u0005\u0000"+
		"\u0000wv\u0001\u0000\u0000\u0000xy\u0001\u0000\u0000\u0000yw\u0001\u0000"+
		"\u0000\u0000yz\u0001\u0000\u0000\u0000z{\u0001\u0000\u0000\u0000{|\u0006"+
		"\u0013\u0000\u0000|(\u0001\u0000\u0000\u0000\t\u0000UY`cikqy\u0001\u0006"+
		"\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}