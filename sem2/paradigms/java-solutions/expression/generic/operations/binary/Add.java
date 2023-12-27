package expression.generic.operations.binary;


import expression.generic.UltimateExpression;
import expression.generic.operations.Operations;

public class Add<T> extends AbstractBinaryOperation<T> {
    private final Operations<T> operationMode;

    public Add(UltimateExpression<T> expression1, UltimateExpression<T> expression2, Operations<T> operationMode) {
        super("+", expression1, expression2);
        this.operationMode = operationMode;
    }

    @Override
    public T evaluate(T arg1, T arg2, T arg3) {
        return operationMode.add(expression1.evaluate(arg1, arg2, arg3), expression2.evaluate(arg1, arg2, arg3));
    }
}
