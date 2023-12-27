package expression;

import expression.operations.binary.AbstractBinaryOperation;

public class Add extends AbstractBinaryOperation {
    public Add(UltimateExpression expression1, UltimateExpression expression2) {
        super("+", expression1, expression2);
    }

    @Override
    public int evaluate(int arg1, int arg2, int arg3) {
        return expression1.evaluate(arg1, arg2, arg3) + expression2.evaluate(arg1, arg2, arg3);
    }
}
