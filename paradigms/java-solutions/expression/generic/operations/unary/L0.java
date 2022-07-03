package expression.generic.operations.unary;

import expression.generic.UltimateExpression;
import expression.generic.operations.Operations;

public class L0<T> extends AbstractUnaryOperation<T> {
    private final Operations<T> operationMode;

    public L0(UltimateExpression<T> expression, Operations<T> operationMode) {
        super("l0", expression);
        this.operationMode = operationMode;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operationMode.l0(expression.evaluate(x, y, z));
    }
}
