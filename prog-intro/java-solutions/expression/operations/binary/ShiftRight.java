package expression.operations.binary;

import expression.UltimateExpression;

public class ShiftRight extends AbstractBinaryOperation {

    public ShiftRight(UltimateExpression expression1, UltimateExpression expression2) {
        super(">>", expression1, expression2);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return expression1.evaluate(x, y, z) >> expression2.evaluate(x, y, z);
    }
}
