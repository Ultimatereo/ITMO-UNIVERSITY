package expression.generic.operations.unary;

import expression.generic.UltimateExpression;
import expression.generic.operations.Operations;

public class T0<T> extends AbstractUnaryOperation<T> {
    private final Operations<T> operationMode;

    public T0(UltimateExpression<T> expression, Operations<T> operationMode) {
        super("t0", expression);
        this.operationMode = operationMode;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operationMode.t0(expression.evaluate(x, y, z));
    }
}
