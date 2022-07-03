package expression.operations.unary;

import expression.UltimateExpression;

import java.util.Objects;

public abstract class AbstractUnaryOperation implements UltimateExpression {
    protected final UltimateExpression expression;
    protected final String sign;

    public AbstractUnaryOperation(String sign, UltimateExpression expression) {
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
        if (o == null || getClass() != o.getClass()) return false;
        AbstractUnaryOperation that = (AbstractUnaryOperation) o;
        return Objects.equals(expression, that.expression) && Objects.equals(sign, that.sign);
    }

    @Override
    public int hashCode() {
        return Objects.hash(expression, sign);
    }


    @Override
    public int evaluate(int x) {
        return evaluate(x, 0, 0);
    }
}
