package expression.generic.operations;

public class LongOperations implements Operations<Long> {
    @Override
    public Long add(Long a, Long b) {
        return a + b;
    }

    @Override
    public Long divide(Long a, Long b) {
        return a / b;
    }

    @Override
    public Long subtract(Long a, Long b) {
        return a - b;
    }

    @Override
    public Long multiply(Long a, Long b) {
        return a * b;
    }

    @Override
    public Long log(Long expr, Long base) {
        if (expr <= 0) {
            throw new ArithmeticException("Logarithmic expression is not greater than zero!");
        }
        if ((base <= 0 || base == 1)) {
            throw new ArithmeticException("Logarithmic base is not greater than zero or equals one!");
        }
        long answer = 1;
        long exp = 0;
        while (answer <= expr) {
            exp += 1;
            long res = answer * base;
            if (res / answer != base) {
                break;
            }
            answer = res;
        }
        return exp - 1;
    }

    @Override
    public Long pow(Long left, Long exp) {
        if (((left == 0) && (exp <= 0))) {
            throw new ArithmeticException("We can't raise 0 to the degree that is not greater than zero");
        }
        if (exp < 0) {
            throw new ArithmeticException("We can't raise to the degree that is lower than zero");
        }
        if ((left == 1 || exp == 0)) {
            return 1L;
        }
        if (left == -1) {
            return (long) (exp % 2 == 0 ? 1 : -1);
        }
        if (left == 0) {
            return 0L;
        }
        long right = left;
        for (long i = 1; i < exp; i++) {
            right = left * right;
        }
        return right;
    }

    @Override
    public Long max(Long a, Long b) {
        return Math.max(a, b);
    }

    @Override
    public Long min(Long a, Long b) {
        return Math.min(a, b);
    }

    @Override
    public Long shiftLeft(Long a, Long b) {
        return a << b;
    }

    @Override
    public Long shiftRight(Long a, Long b) {
        return a >> b;
    }

    @Override
    public Long shiftRightRight(Long a, Long b) {
        return a >>> b;
    }

    @Override
    public Long parseConst(String constant) {
        return Long.parseLong(constant);
    }

    @Override
    public Long negate(Long a) {
        return -a;
    }

    @Override
    public Long l0(Long res) {
        String bin = Long.toBinaryString(res);
        return res == 0 ? 64L : 64 - bin.length();
    }

    @Override
    public Long t0(Long res) {
        String bin = Long.toBinaryString(res);
        int ans = 0;
        while (ans < bin.length()) {
            if (bin.charAt(bin.length() - 1 - ans) == '0') {
                ans++;
            } else {
                break;
            }
        }
        return res == 0 ? 64L : ans;
    }

    @Override
    public Long abs(Long value) {
        if (value < 0) {
            return -value;
        }
        return value;
    }

    @Override
    public Long count(Long a) {
        return (long) Long.bitCount(a);
    }
}
