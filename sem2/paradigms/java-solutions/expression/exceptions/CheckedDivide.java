package expression.exceptions;

import expression.Divide;
import expression.UltimateExpression;

public class CheckedDivide extends Divide {

    public CheckedDivide(UltimateExpression expression1, UltimateExpression expression2) {
        super(expression1, expression2);
    }

    @Override
    public int evaluate(int arg1, int arg2, int arg3) {
        final int left = expression1.evaluate(arg1, arg2, arg3);
        final int right = expression2.evaluate(arg1, arg2, arg3);
        if (right == 0) {
            throw new ArithmeticException("Division by zero");
        }
        if (left == Integer.MIN_VALUE && right == -1) {
            throw new ArithmeticException("Overflow");
        }
        return left / right;
    }
}
