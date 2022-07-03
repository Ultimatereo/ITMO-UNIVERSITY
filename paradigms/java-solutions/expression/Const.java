package expression;

public class Const extends AbstractValue {
    private final Object constant;

    public Const(int constant) {
        super(String.valueOf(constant));
        this.constant = constant;
    }


    @Override
    public int evaluate(int arg1, int arg2, int arg3) {
        return (int) constant;
    }

}
