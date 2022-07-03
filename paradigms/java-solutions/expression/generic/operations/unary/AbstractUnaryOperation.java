package expression.generic.operations.unary;

import expression.generic.UltimateExpression;

import java.util.Objects;

public abstract class AbstractUnaryOperation<T> implements UltimateExpression<T> {
    protected final UltimateExpression<T> expression;
    protected final String sign;

    public AbstractUnaryOperation(String sign, UltimateExpression<T> expression) {
        this.sign = sign;
        this.expression = expression;
    }

    @Override
    public String toString() {
        return sign + "(" + expression + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof AbstractUnaryOperation that)) return false;
        return Objects.equals(sign, that.sign) && Objects.equals(expression, that.expression);
    }

    @Override
    public int hashCode() {
        return Objects.hash(expression, sign);
    }


    @Override
    public T evaluate(T x) {
        return evaluate(x, null, null);
    }
}
