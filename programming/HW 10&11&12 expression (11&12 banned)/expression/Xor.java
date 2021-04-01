package expression;

public class Xor extends AbstractOperation {

    public Xor(final HelpfulInterface helpfulInterface1, final HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "^");
    }

    @Override
    public int eval(int x, int y) {
        return x ^ y;
    }
}
