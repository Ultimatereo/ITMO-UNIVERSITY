package expression;

import java.util.Objects;

public class Variable implements HelpfulInterface {
    private final String variable;

    public Variable(String variable) {
        this.variable = variable;
    }

    @Override
    public int evaluate(int value) {
        return value;
    }
    @Override
    public int evaluate(int value1, int value2, int value3) {
        switch (variable) {
            case "x":
                return value1;
            case "y":
                return value2;
            default:
                return value3;
        }
    }

    public String toString() {
        return variable;
    }
    @Override
    public boolean equals(final Object obj) {
        if (obj instanceof Variable) {
            Variable that = (Variable) obj;
            return this.variable.equals(that.variable);
        }
        return false;
    }

    final public int hashCode() {
        return Objects.hash(variable, getClass());
    }

}