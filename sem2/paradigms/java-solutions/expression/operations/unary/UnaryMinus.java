package expression.operations.unary;

import expression.UltimateExpression;

public class UnaryMinus extends AbstractUnaryOperation {

    public UnaryMinus(UltimateExpression expression) {
        super("-", expression);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return -expression.evaluate(x, y, z);
    }
}
