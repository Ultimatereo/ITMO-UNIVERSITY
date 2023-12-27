package expression.operations.binary;

import expression.UltimateExpression;

public class Min extends AbstractBinaryOperation {
    public Min(UltimateExpression expression1, UltimateExpression expression2) {
        super("min", expression1, expression2);
    }


    @Override
    public int evaluate(int x, int y, int z) {
        return Math.min(expression1.evaluate(x, y, z), expression2.evaluate(x, y, z));
    }
}
