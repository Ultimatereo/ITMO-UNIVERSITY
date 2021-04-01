package expression;

public class Or extends AbstractOperation {

    public Or(final HelpfulInterface helpfulInterface1, final HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "|");
    }

    @Override
    public int eval(int x, int y) {
        return x | y;
    }
}
