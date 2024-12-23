package lexic.token.token;

public abstract class TokenContainer implements Token {
    private final String value;

    public TokenContainer(String value) {
        this.value = value;
    }


    @Override
    public String value() {
        return value;
    }
}
