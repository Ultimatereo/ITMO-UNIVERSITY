package expression.operations.unary;

import expression.UltimateExpression;

public class CheckedAbs extends AbstractUnaryOperation {
    public CheckedAbs(UltimateExpression expression) {
        super("abs", expression);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        final int value = expression.evaluate(x, y, z);
        if (value == Integer.MIN_VALUE) {
            throw new ArithmeticException("Overflow");
        }
        if (value < 0) {
            return -value;
        }
        return value;
    }
}
