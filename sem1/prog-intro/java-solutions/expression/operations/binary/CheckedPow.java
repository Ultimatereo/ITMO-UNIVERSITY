package expression.operations.binary;

import expression.UltimateExpression;


public class CheckedPow extends AbstractBinaryOperation {

    public CheckedPow(UltimateExpression expression1, UltimateExpression expression2) {
        super("**", expression1, expression2);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        final int left = expression1.evaluate(x, y, z);
        final int exp = expression2.evaluate(x, y, z);
        if ((left == 0) && (exp <= 0)) {
            throw new ArithmeticException("We can't raise 0 to the degree that is not greater than zero");
        }
        if (exp < 0) {
            throw new ArithmeticException("We can't raise to the degree that is lower than zero");
        }
        if (left == 1 || exp == 0) {
            return 1;
        }
        if (left == -1) {
            return exp % 2 == 0 ? 1 : -1;
        }
        if (left == 0) {
            return 0;
        }
        int right = left;
        for (int i = 1; i < exp; i++) {
            int mul = left * right;
            if (mul / left != right) {
                throw new ArithmeticException("Overflow");
            }
            right = mul;
        }
        return right;
    }
}
