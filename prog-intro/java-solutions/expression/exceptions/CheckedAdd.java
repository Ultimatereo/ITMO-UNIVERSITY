package expression.exceptions;

import expression.Add;
import expression.UltimateExpression;

public class CheckedAdd extends Add {
    public CheckedAdd(UltimateExpression expression1, UltimateExpression expression2) {
        super(expression1, expression2);
    }

    @Override
    public int evaluate(int arg1, int arg2, int arg3) {
        final int left = expression1.evaluate(arg1, arg2, arg3);
        final int right = expression2.evaluate(arg1, arg2, arg3);
        //System.err.println(left + " " + right);
        final int sum = left + right;
        //System.err.println(left + " " + right + " " + sum);
        if ((((left & right & ~sum) < 0) || ((~left & ~right & sum)) < 0)) {
            // 00 1 is bad
            // 01 It's fine
            // 10 It's fine
            // 11 0 is bad
            throw new ArithmeticException("Overflow");
        }
        return sum;
    }

}
