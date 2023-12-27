package expression.operations.binary;

import expression.UltimateExpression;

public class CheckedLog extends AbstractBinaryOperation {

    public CheckedLog(UltimateExpression expression1, UltimateExpression expression2) {
        super("//", expression1, expression2);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int expr = expression1.evaluate(x, y, z);
        int base = expression2.evaluate(x, y, z);
        if (expr <= 0) {
            throw new ArithmeticException("Logarithmic expression is not greater than zero!");
        }
        if (base <= 0 || base == 1) {
            throw new ArithmeticException("Logarithmic base is not greater than zero or equals one!");
        }
        int answer = 1;
        int exp = 0;
        while (answer <= expr) {
            exp += 1;
            int res = answer * base;
            if (res / answer != base) {
                break;
            }
            answer = res;
        }
        return exp - 1;
    }
}
