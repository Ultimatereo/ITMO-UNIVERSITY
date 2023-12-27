package expression.generic.operations;

public class ShortOperations implements Operations<Short> {
    @Override
    public Short add(Short a, Short b) {
        return (short) (a + b);
    }

    @Override
    public Short divide(Short a, Short b) {
        return (short) (a / b);
    }

    @Override
    public Short subtract(Short a, Short b) {
        return (short) (a - b);
    }

    @Override
    public Short multiply(Short a, Short b) {
        return (short) (a * b);
    }

    @Override
    public Short log(Short expr, Short base) {
        if (expr <= 0) {
            throw new ArithmeticException("Logarithmic expression is not greater than zero!");
        }
        if ((base <= 0 || base == 1)) {
            throw new ArithmeticException("Logarithmic base is not greater than zero or equals one!");
        }
        short answer = 1;
        short exp = 0;
        while (answer <= expr) {
            exp += 1;
            short res = (short) (answer * base);
            if (res / answer != base) {
                break;
            }
            answer = res;
        }
        return (short) (exp - 1);
    }

    @Override
    public Short pow(Short left, Short exp) {
        if (((left == 0) && (exp <= 0))) {
            throw new ArithmeticException("We can't raise 0 to the degree that is not greater than zero");
        }
        if (exp < 0) {
            throw new ArithmeticException("We can't raise to the degree that is lower than zero");
        }
        if ((left == 1 || exp == 0)) {
            return 1;
        }
        if (left == -1) {
            return exp % 2 == 0 ? (short) 1 : -1;
        }
        if (left == 0) {
            return 0;
        }
        short right = left;
        for (long i = 1; i < exp; i++) {
            right = (short) (left * right);
        }
        return right;
    }

    @Override
    public Short max(Short a, Short b) {
        return (short) Math.max(a, b);
    }

    @Override
    public Short min(Short a, Short b) {
        return (short) Math.min(a, b);
    }

    @Override
    public Short shiftLeft(Short a, Short b) {
        return (short) (a << b);
    }

    @Override
    public Short shiftRight(Short a, Short b) {
        return (short) (a >> b);
    }

    @Override
    public Short shiftRightRight(Short a, Short b) {
        return (short) (a >>> b);
    }

    @Override
    public Short parseConst(String constant) {
        return (short) Integer.parseInt(constant);
    }

    @Override
    public Short negate(Short a) {
        return (short) (-a);
    }

    @Override
    public Short l0(Short res) {
        String bin = Integer.toBinaryString(res);
        return res == 0 ? (short) 16 : (short) (16 - bin.length());
    }

    @Override
    public Short t0(Short res) {
        String bin = Integer.toBinaryString(res);
        short ans = 0;
        while (ans < bin.length()) {
            if (bin.charAt(bin.length() - 1 - ans) == '0') {
                ans++;
            } else {
                break;
            }
        }
        return res == 0 ? 16 : ans;
    }

    @Override
    public Short abs(Short value) {
        if (value < 0) {
            return (short) (-value);
        }
        return value;
    }

    @Override
    public Short count(Short a) {
        short count = 0;
        for (int i = 0; i < 16; i++) {
            if ((a & (1 << i)) != 0) {
                count++;
            }
        }
        return count;
    }
}
