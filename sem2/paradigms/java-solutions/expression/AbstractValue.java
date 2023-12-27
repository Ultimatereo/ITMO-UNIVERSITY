package expression;

import java.util.Objects;

public abstract class AbstractValue implements UltimateExpression {
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
        if (!(o instanceof AbstractValue)) return false;
        AbstractValue that = (AbstractValue) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public int evaluate(int x) {
        return evaluate(x, 0, 0);
    }
}
