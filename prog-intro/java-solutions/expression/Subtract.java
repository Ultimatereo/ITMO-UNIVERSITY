package expression;

import expression.UltimateExpression;
import expression.operations.binary.AbstractBinaryOperation;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Subtract extends AbstractBinaryOperation {
    public Subtract(UltimateExpression expression1, UltimateExpression expression2) {
        super("-", expression1, expression2);
    }

    @Override
    public int evaluate(int arg1, int arg2, int arg3) {
        return expression1.evaluate(arg1, arg2, arg3) - expression2.evaluate(arg1, arg2, arg3);
    }

    @Override
    public BigInteger evaluate(BigInteger x) {
        return expression1.evaluate(x).subtract(expression2.evaluate(x));
    }

    @Override
    public BigDecimal evaluate(BigDecimal x) {
        return expression1.evaluate(x).subtract(expression2.evaluate(x));
    }
}
