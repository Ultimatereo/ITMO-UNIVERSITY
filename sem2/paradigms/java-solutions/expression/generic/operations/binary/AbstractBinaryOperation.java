package expression.generic.operations.binary;


import expression.generic.UltimateExpression;

import java.util.Objects;

public abstract class AbstractBinaryOperation<T> implements UltimateExpression<T> {
    protected final String operation;
    protected final UltimateExpression<T> expression1;
    protected final UltimateExpression<T> expression2;

    public AbstractBinaryOperation(String operation, UltimateExpression<T> expression1, UltimateExpression<T> expression2) {
        this.operation = operation;
        this.expression1 = expression1;
        this.expression2 = expression2;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof AbstractBinaryOperation that)) return false;
        return Objects.equals(operation, that.operation) && Objects.equals(expression1, that.expression1) && Objects.equals(expression2, that.expression2);
    }

    @Override
    public int hashCode() {
        return Objects.hash(expression1, expression2, operation);
    }

    public String toString() {
        return "(" +
                expression1.toString() +
                " " +
                operation +
                " " +
                expression2.toString() +
                ")";
    }


    @Override
    public T evaluate(T x) {
        return evaluate(x, null, null);
    }
}
