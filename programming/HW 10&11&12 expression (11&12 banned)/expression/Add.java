package expression;


public class Add extends AbstractOperation {
    public Add(HelpfulInterface helpfulInterface1, HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "+");
    }
    @Override
    public int eval(int x, int y) {
        return x + y;
    }
}
