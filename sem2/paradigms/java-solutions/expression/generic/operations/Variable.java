package expression.generic.operations;

public class Variable<T> extends AbstractValue<T> {
    private final String var;

    public Variable(String var) {
        super(var);
        this.var = var;
    }

    @Override
    public T evaluate(T arg1, T arg2, T arg3) {
        return switch (var) {
            case "x" -> arg1;
            case "y" -> arg2;
            case "z" -> arg3;
            default -> throw new AssertionError("There are more variables aside from x, y and z! You need also give them values.");
        };
    }
}
