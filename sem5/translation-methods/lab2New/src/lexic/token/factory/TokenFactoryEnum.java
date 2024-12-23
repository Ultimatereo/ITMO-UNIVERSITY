package lexic.token.factory;

import lexic.token.token.Name;
import lexic.token.token.TokenEnum;
import lexic.token.token.Value;

public enum TokenFactoryEnum {
    NAME_FACTORY(new TokenRegexFactory("[a-zA-Z_][a-zA-Z0-9_]*",
            (String val) -> {
                TokenEnum token = TokenEnum.NAME;
                token.setToken(new Name(val));
                return token;
            })),
    VALUE_FACTORY(new TokenRegexFactory("[0-9]+[lLfF]?|\"[a-zA-Z_][a-zA-Z0-9_]*\"|'.'",
            (String val) -> {
                TokenEnum token = TokenEnum.VALUE;
                token.setToken(new Value(val));
                return token;
            }));

    private final TokenRegexFactory tokenFactory;

    TokenFactoryEnum(TokenRegexFactory tokenFactory) {
        this.tokenFactory = tokenFactory;
    }

    public TokenRegexFactory tokenFactory() {
        return tokenFactory;
    }
}
