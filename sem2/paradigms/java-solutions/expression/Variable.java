package expression;

public class Variable extends AbstractValue {
    private final String var;

    public Variable(String var) {
        super(var);
        this.var = var;
    }

    @Override
    public int evaluate(int arg1, int arg2, int arg3) {
        return switch (var) {
            case "x" -> arg1;
            case "y" -> arg2;
            case "z" -> arg3;
            default -> throw new AssertionError("There are more variables aside from x, y and z! You need also give them values.");
        };
    }
}
