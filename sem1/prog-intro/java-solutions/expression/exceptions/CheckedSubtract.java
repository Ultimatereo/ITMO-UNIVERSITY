package expression.exceptions;

import expression.Subtract;
import expression.UltimateExpression;

public class CheckedSubtract extends Subtract {

    public CheckedSubtract(UltimateExpression expression1, UltimateExpression expression2) {
        super(expression1, expression2);
    }

    @Override
    public int evaluate(int arg1, int arg2, int arg3) {
        final int left = expression1.evaluate(arg1, arg2, arg3);
        final int right = expression2.evaluate(arg1, arg2, arg3);
        final int dif = left - right;
        if ((((left & ~right & ~dif) < 0) || ((~left & right & dif)) < 0)) {
            //00 No problem
            //01 If 1 it's bad
            //10 If 0 it's bad
            //11 No problem
            throw new ArithmeticException("Overflow");
        }
        return dif;
    }
}
