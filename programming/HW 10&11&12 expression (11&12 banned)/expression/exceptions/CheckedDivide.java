package expression.exceptions;

import expression.AbstractOperation;
import expression.HelpfulInterface;

public class CheckedDivide extends AbstractOperation {

    public CheckedDivide(final HelpfulInterface helpfulInterface1, final HelpfulInterface helpfulInterface2) {
        super(helpfulInterface1, helpfulInterface2, "/");
    }

    @Override
    public int eval(final int x, final int y) {
        if (y == 0) {
            throw new ArithmeticException("Division by zero");
        }
        if (x == Integer.MIN_VALUE && y == -1) {
            throw new ArithmeticException("Overflow");
        }
        return x / y;
    }
}
