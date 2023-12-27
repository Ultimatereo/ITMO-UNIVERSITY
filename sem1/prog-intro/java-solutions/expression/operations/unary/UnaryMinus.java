package expression.operations.unary;

import expression.UltimateExpression;

import java.math.BigDecimal;
import java.math.BigInteger;

public class UnaryMinus extends AbstractUnaryOperation {

    public UnaryMinus(UltimateExpression expression) {
        super("-", expression);
    }

    @Override
    public BigDecimal evaluate(BigDecimal x) {
        return BigDecimal.ZERO.subtract(expression.evaluate(x));
    }

    @Override
    public BigInteger evaluate(BigInteger x) {
        return BigInteger.ZERO.subtract(expression.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return -expression.evaluate(x, y, z);
    }
}
