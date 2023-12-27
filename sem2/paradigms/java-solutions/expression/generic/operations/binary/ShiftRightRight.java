package expression.generic.operations.binary;

import expression.generic.UltimateExpression;
import expression.generic.operations.Operations;

public class ShiftRightRight<T> extends AbstractBinaryOperation<T> {
    private final Operations<T> operationMode;

    public ShiftRightRight(UltimateExpression<T> expression1, UltimateExpression<T> expression2, Operations<T> operationMode) {
        super(">>>", expression1, expression2);
        this.operationMode = operationMode;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operationMode.shiftRightRight(expression1.evaluate(x, y, z), expression2.evaluate(x, y, z));
    }
}
