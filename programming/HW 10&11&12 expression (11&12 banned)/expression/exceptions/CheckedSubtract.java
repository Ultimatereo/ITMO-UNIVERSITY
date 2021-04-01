package expression.exceptions;

import expression.AbstractOperation;
import expression.HelpfulInterface;

public class CheckedSubtract extends AbstractOperation {

    public CheckedSubtract(final HelpfulInterface helpfulInterface1, final HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "-");
    }

    @Override
    public int eval(final int x, final int y) {
        int r = x - y;
        if (((x ^ y) & (x ^ r)) < 0) {
            throw new ArithmeticException("Overflow");
        }
        return r;
    }
}
