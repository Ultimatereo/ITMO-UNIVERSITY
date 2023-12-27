package expression.generic.operations;

import java.math.BigInteger;

public class BigIntegerOperations implements Operations<BigInteger> {

    @Override
    public BigInteger add(BigInteger a, BigInteger b) {
        return a.add(b);
    }

    @Override
    public BigInteger divide(BigInteger a, BigInteger b) {
        return a.divide(b);
    }

    @Override
    public BigInteger subtract(BigInteger a, BigInteger b) {
        return a.subtract(b);
    }

    @Override
    public BigInteger multiply(BigInteger a, BigInteger b) {
        return a.multiply(b);
    }

    @Override
    public BigInteger log(BigInteger expr, BigInteger base) {
        if (expr.compareTo(BigInteger.ZERO) <= 0) {
            throw new ArithmeticException("Logarithmic expression is not greater than zero!");
        }
        if (base.compareTo(BigInteger.ZERO) <= 0 || base.compareTo(BigInteger.ONE) == 0) {
            throw new ArithmeticException("Logarithmic base is not greater than zero or equals one!");
        }
        BigInteger answer = BigInteger.ONE;
        BigInteger exp = BigInteger.ZERO;
        while (answer.compareTo(expr) <= 0) {
            exp = exp.add(BigInteger.ONE);
            BigInteger res = BigInteger.ONE;
            res = res.multiply(answer).multiply(base);
            BigInteger temp = BigInteger.ONE;
            temp = temp.multiply(res).divide(answer);
            if (temp.compareTo(base) != 0) {
                break;
            }
            answer = res;
        }
        return exp.subtract(BigInteger.ONE);
    }

    @Override
    public BigInteger pow(BigInteger left, BigInteger exp) {
        if ((left.compareTo(BigInteger.ZERO) == 0) && (exp.compareTo(BigInteger.ZERO) <= 0)) {
            throw new ArithmeticException("We can't raise 0 to the degree that is not greater than zero");
        }
        if (exp.compareTo(BigInteger.ZERO) < 0) {
            throw new ArithmeticException("We can't raise to the degree that is lower than zero");
        }
        if (left.compareTo(BigInteger.ONE) == 0 || exp.compareTo(BigInteger.ZERO) == 0) {
            return BigInteger.ONE;
        }
        if (left.compareTo(BigInteger.ZERO.subtract(BigInteger.ONE)) == 0) {
            return exp.mod(BigInteger.TWO).equals(BigInteger.ZERO) ? BigInteger.ONE : BigInteger.ZERO.subtract(BigInteger.ONE);
        }
        if (left.compareTo(BigInteger.ZERO) == 0) {
            return BigInteger.ZERO;
        }
        BigInteger right = left;
        for (BigInteger i = BigInteger.ONE; i.compareTo(exp) < 0; i = i.add(BigInteger.ONE)) {
            BigInteger mul = left.multiply(right);
            if (mul.divide(left).compareTo(right) != 0) {
                throw new ArithmeticException("Overflow");
            }
            right = mul;
        }
        return right;
    }

    @Override
    public BigInteger max(BigInteger a, BigInteger b) {
        return a.compareTo(b) > 0 ? a : b;
    }

    @Override
    public BigInteger min(BigInteger a, BigInteger b) {
        return a.compareTo(b) < 0 ? a : b;
    }

    @Override
    public BigInteger shiftLeft(BigInteger a, BigInteger b) {
        return null;
    }

    @Override
    public BigInteger shiftRight(BigInteger a, BigInteger b) {
        return null;
    }

    @Override
    public BigInteger shiftRightRight(BigInteger a, BigInteger b) {
        return null;
    }

    @Override
    public BigInteger negate(BigInteger a) {
        return BigInteger.ZERO.subtract(a);
    }

    @Override
    public BigInteger l0(BigInteger a) {
        return null;
    }

    @Override
    public BigInteger t0(BigInteger a) {
        return null;
    }

    @Override
    public BigInteger abs(BigInteger a) {
        return a.compareTo(BigInteger.ZERO) > 0 ? a : BigInteger.ZERO.subtract(a);
    }

    @Override
    public BigInteger count(BigInteger a) {
        return BigInteger.valueOf(a.bitCount());
    }

    @Override
    public BigInteger parseConst(String constant) {
        return new BigInteger(constant);
    }
}
