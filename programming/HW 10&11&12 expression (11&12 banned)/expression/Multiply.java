package expression;

public class Multiply extends AbstractOperation {
    public Multiply(HelpfulInterface helpfulInterface1, HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "*");
    }
    @Override
    public int eval(int value1, int value2) {
        return value1 * value2;
    }
}
