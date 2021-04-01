package expression;

public class Subtract extends AbstractOperation {

    public Subtract(HelpfulInterface helpfulInterface1, HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "-");
    }

    @Override
    protected int eval(int value1, int value2) {
        return value1 - value2;
    }

}
