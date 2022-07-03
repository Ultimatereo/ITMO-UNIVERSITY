package expression.operations.binary;

import expression.UltimateExpression;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

public abstract class AbstractBinaryOperation implements UltimateExpression {
    protected final String operation;
    protected final UltimateExpression expression1;
    protected final UltimateExpression expression2;

    public AbstractBinaryOperation(String operation, UltimateExpression expression1, UltimateExpression expression2) {
        this.operation = operation;
        this.expression1 = expression1;
        this.expression2 = expression2;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof AbstractBinaryOperation)) return false;
        AbstractBinaryOperation that = (AbstractBinaryOperation) o;
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
    public BigDecimal evaluate(BigDecimal x) {
        return null;
    }

    @Override
    public BigInteger evaluate(BigInteger x) {
        return null;
    }

    @Override
    public int evaluate(int x) {
        return evaluate(x, 0, 0);
    }
}
