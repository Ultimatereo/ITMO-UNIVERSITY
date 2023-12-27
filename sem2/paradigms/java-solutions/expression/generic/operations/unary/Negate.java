package expression.generic.operations.unary;

import expression.generic.UltimateExpression;
import expression.generic.operations.Operations;

public class Negate<T> extends AbstractUnaryOperation<T> {
    private final Operations<T> operationMode;

    public Negate(UltimateExpression<T> expression, Operations<T> operationMode) {
        super("-", expression);
        this.operationMode = operationMode;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operationMode.negate(expression.evaluate(x, y, z));
    }
}
