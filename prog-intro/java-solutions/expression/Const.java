package expression;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Const extends AbstractValue {
    private final Object constant;

    public Const(int constant) {
        super(String.valueOf(constant));
        this.constant = constant;
    }

    public Const(BigInteger constant) {
        super(String.valueOf(constant));
        this.constant = constant;
    }

    public Const(BigDecimal constant) {
        super(String.valueOf(constant));
        this.constant = constant;
    }

    @Override
    public int evaluate(int arg1, int arg2, int arg3) {
        return (int) constant;
    }

    @Override
    public BigInteger evaluate(BigInteger x) {
        return (BigInteger) constant;
    }

    @Override
    public BigDecimal evaluate(BigDecimal x) {
        return (BigDecimal) constant;
    }
}
