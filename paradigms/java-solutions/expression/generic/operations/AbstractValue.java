package expression.generic.operations;

import expression.generic.UltimateExpression;

import java.util.Objects;

public abstract class AbstractValue<T> implements UltimateExpression<T> {
    protected String value;

    public AbstractValue(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof AbstractValue that)) return false;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public T evaluate(T x) {
        return evaluate(x, null, null);
    }
}
