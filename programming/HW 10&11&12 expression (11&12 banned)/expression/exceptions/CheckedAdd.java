package expression.exceptions;

import expression.AbstractOperation;
import expression.HelpfulInterface;

public class CheckedAdd extends AbstractOperation {

    public CheckedAdd(final HelpfulInterface helpfulInterface1, final HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "+");
    }

    @Override
    public int eval(final int x, final int y) {
        int r = x + y;
        if (((x ^ r) & (y ^ r)) < 0) {
            throw new ArithmeticException("Overflow");
        }
        return r;
    }
}