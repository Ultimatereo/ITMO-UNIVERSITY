package expression.generic.operations;

public class Const<T> extends AbstractValue<T> {
    private final T constant;

    public Const(T constant) {
        super(String.valueOf(constant));
        this.constant = constant;
    }


    @Override
    public T evaluate(T arg1, T arg2, T arg3) {
        return constant;
    }

}
