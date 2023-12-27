package expression.generic.operations.unary;

import expression.generic.UltimateExpression;
import expression.generic.operations.Operations;

public class Abs<T> extends AbstractUnaryOperation<T> {
    private final Operations<T> operationMode;

    public Abs(UltimateExpression<T> expression, Operations<T> operationMode) {
        super("abs", expression);
        this.operationMode = operationMode;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operationMode.abs(expression.evaluate(x, y, z));
    }
}
