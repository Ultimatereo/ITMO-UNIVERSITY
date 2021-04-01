package expression;

import java.util.Objects;

public class Const implements HelpfulInterface {
    private final int constValue;

    public Const(int constValue) {
        this.constValue = constValue;
    }
    @Override
    public int evaluate(int value) {
        return this.constValue;
    }
    @Override
    public String toString() {
        return String.valueOf(this.constValue);
    }
    @Override
    public int evaluate(int value1, int value2, int value3) {
        return this.constValue;
    }
    @Override
    public boolean equals(Object object) {
        if (object instanceof Const) {
            Const that = (Const) object;
            return this.constValue == that.constValue;
        }
        return false;
    }
    @Override
    final public int hashCode() {
        return Objects.hash(constValue, getClass());
    }

}