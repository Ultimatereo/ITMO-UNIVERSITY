package expression.exceptions;

import expression.AbstractOperation;
import expression.HelpfulInterface;

public class CheckedMultiply extends AbstractOperation {
    public CheckedMultiply(final HelpfulInterface helpfulInterface1, final HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "*");
    }

    @Override
    public int eval(final int x, final int y) {
        int r = x * y;
        if (((y != 0) && (r / y != x)) || (x == Integer.MIN_VALUE && y == -1)) {
            throw new ArithmeticException("Overflow");
        }
        return r;
    }
}
