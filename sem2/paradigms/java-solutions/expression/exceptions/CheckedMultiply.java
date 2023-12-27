package expression.exceptions;

import expression.Multiply;
import expression.UltimateExpression;

public class CheckedMultiply extends Multiply {

    public CheckedMultiply(UltimateExpression expression1, UltimateExpression expression2) {
        super(expression1, expression2);
    }

    @Override
    public int evaluate(int arg1, int arg2, int arg3) {
        final int left = expression1.evaluate(arg1, arg2, arg3);
        final int right = expression2.evaluate(arg1, arg2, arg3);
        int mul = left * right;
        if ((left != 0 && mul / left != right) || (left == -1 && right == Integer.MIN_VALUE)) {
            throw new ArithmeticException("Overflow");
        }
        return left * right;
    }
}
