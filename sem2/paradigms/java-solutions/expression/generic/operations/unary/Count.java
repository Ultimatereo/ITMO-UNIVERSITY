package expression.generic.operations.unary;

import expression.generic.UltimateExpression;
import expression.generic.operations.Operations;

public class Count<T> extends AbstractUnaryOperation<T> {
    private final Operations<T> operationMode;

    public Count(UltimateExpression<T> expression, Operations<T> operationMode) {
        super("count", expression);
        this.operationMode = operationMode;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operationMode.count(expression.evaluate(x, y, z));
    }
}
