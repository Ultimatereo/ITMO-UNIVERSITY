package lexic.token.token;

public enum TokenEnum {
    FUN(new KeyWord("fun")),
    INLINE(new KeyWord("inline")),
    INFIX(new KeyWord("infix")),
    PUBLIC(new KeyWord("public")),
    PRIVATE(new KeyWord("private")),
    PROTECTED(new KeyWord("protected")),
    INTERNAL(new KeyWord("internal")),
    OPEN(new KeyWord("open")),
    LPAREN(new Symbol("(")),
    RPAREN(new Symbol(")")),
    COLON(new Symbol(":")),
    COMMA(new Symbol(",")),
    LCHEVRON(new Symbol("<")),
    RCHEVRON(new Symbol(">")),
    ARROW(new Symbol("->")),
    DOT(new Symbol(".")),
    QUESTION(new Symbol("?")),
    STAR(new Symbol("*")),
    EQUALS(new Symbol("=")),
    END(new End()),
    NAME,
    VALUE;

    private Token token;

    TokenEnum(Token token) {
        this.token = token;
    }

    TokenEnum() {
        this.token = null;
    }

    public void setToken(Token token) {
        this.token = token;
    }

    public Token token() {
        return token;
    }
}
