package lexic.token.factory;

import lexic.token.token.TokenEnum;

import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TokenRegexFactory {
    private final Pattern pattern;
    private final Function<String, TokenEnum> tokenGenerator;

    public TokenRegexFactory(String regex, Function<String, TokenEnum> tokenGenerator) {
        this.pattern = Pattern.compile(regex);
        this.tokenGenerator = tokenGenerator;
    }

    public TokenEnum createToken(String val) {
        Matcher matcher = pattern.matcher(val);
        if (matcher.matches()) {
            return tokenGenerator.apply(val);
        }
        return null;
    }
}
