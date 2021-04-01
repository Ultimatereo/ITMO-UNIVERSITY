package expression;

public class And extends AbstractOperation {

    public And(HelpfulInterface helpfulInterface1, HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "&");
    }

    @Override
    public int eval(int x, int y) {
        return x & y;
    }
}