package expression.operations.binary;

import expression.UltimateExpression;

public class Max extends AbstractBinaryOperation {
    public Max(UltimateExpression expression1, UltimateExpression expression2) {
        super("max", expression1, expression2);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return Math.max(expression1.evaluate(x, y, z), expression2.evaluate(x, y, z));
    }
}
