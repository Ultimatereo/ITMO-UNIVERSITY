package expression.operations.binary;

import expression.UltimateExpression;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Max extends AbstractBinaryOperation {
    public Max(UltimateExpression expression1, UltimateExpression expression2) {
        super("max", expression1, expression2);
    }

    @Override
    public BigDecimal evaluate(BigDecimal x) {
        BigDecimal res1 = expression1.evaluate(x);
        BigDecimal res2 = expression2.evaluate(x);
        if (res1.compareTo(res2) > 0) {
            return res1;
        } else {
            return res2;
        }
    }

    @Override
    public BigInteger evaluate(BigInteger x) {
        BigInteger res1 = expression1.evaluate(x);
        BigInteger res2 = expression2.evaluate(x);
        if (res1.compareTo(res2) > 0) {
            return res1;
        } else {
            return res2;
        }
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return Math.max(expression1.evaluate(x, y, z), expression2.evaluate(x, y, z));
    }
}
