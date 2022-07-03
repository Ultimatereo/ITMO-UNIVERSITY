package expression.exceptions;

import expression.UltimateExpression;
import expression.operations.unary.UnaryMinus;

public class CheckedNegate extends UnaryMinus {

    public CheckedNegate(UltimateExpression expression) {
        super(expression);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        final int exp = expression.evaluate(x, y, z);
        if (exp == Integer.MIN_VALUE) {
            throw new ArithmeticException("Overflow");
        }
        return -exp;
    }
}
