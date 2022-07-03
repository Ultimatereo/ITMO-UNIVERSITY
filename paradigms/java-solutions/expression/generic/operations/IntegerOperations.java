package expression.generic.operations;

public class IntegerOperations implements Operations<Integer> {
    private final boolean isChecked;

    public IntegerOperations(boolean isChecked) {
        this.isChecked = isChecked;
    }


    @Override
    public Integer add(Integer left, Integer right) {
        final int sum = left + right;
        //System.err.println(left + " " + right + " " + sum);
        if (isChecked && (((left & right & ~sum) < 0) || ((~left & ~right & sum) < 0))) {
            // 00 1 is bad
            // 01 It's fine
            // 10 It's fine
            // 11 0 is bad
            throw new ArithmeticException("Overflow");
        }
        return sum;
    }

    @Override
    public Integer divide(Integer left, Integer right) {
        if (isChecked && right == 0) {
            throw new ArithmeticException("Division by zero");
        }
        if (isChecked && left == Integer.MIN_VALUE && right == -1) {
            throw new ArithmeticException("Overflow");
        }
        return left / right;
    }

    @Override
    public Integer subtract(Integer left, Integer right) {
        final int dif = left - right;
        if (isChecked && (((left & ~right & ~dif) < 0) || ((~left & right & dif) < 0))) {
            //00 No problem
            //01 If 1 it's bad
            //10 If 0 it's bad
            //11 No problem
            throw new ArithmeticException("Overflow");
        }
        return dif;
    }

    @Override
    public Integer multiply(Integer left, Integer right) {
        int mul = left * right;
        if (isChecked &&
                ((left != 0 && mul / left != right) || (left == -1 && right == Integer.MIN_VALUE))) {
            throw new ArithmeticException("Overflow");
        }
        return left * right;
    }

    @Override
    public Integer log(Integer expr, Integer base) {
        if (isChecked && expr <= 0) {
            throw new ArithmeticException("Logarithmic expression is not greater than zero!");
        }
        if (isChecked && (base <= 0 || base == 1)) {
            throw new ArithmeticException("Logarithmic base is not greater than zero or equals one!");
        }
        int answer = 1;
        int exp = 0;
        while (answer <= expr) {
            exp += 1;
            int res = answer * base;
            if (res / answer != base) {
                break;
            }
            answer = res;
        }
        return exp - 1;
    }

    @Override
    public Integer pow(Integer left, Integer exp) {
        if (isChecked && ((left == 0) && (exp <= 0))) {
            throw new ArithmeticException("We can't raise 0 to the degree that is not greater than zero");
        }
        if (isChecked && exp < 0) {
            throw new ArithmeticException("We can't raise to the degree that is lower than zero");
        }
        if (isChecked && (left == 1 || exp == 0)) {
            return 1;
        }
        if (isChecked && left == -1) {
            return exp % 2 == 0 ? 1 : -1;
        }
        if (isChecked && left == 0) {
            return 0;
        }
        int right = left;
        for (int i = 1; i < exp; i++) {
            int mul = left * right;
            if (isChecked && mul / left != right) {
                throw new ArithmeticException("Overflow");
            }
            right = mul;
        }
        return right;
    }

    @Override
    public Integer max(Integer a, Integer b) {
        return Math.max(a, b);
    }

    @Override
    public Integer min(Integer a, Integer b) {
        return Math.min(a, b);
    }

    @Override
    public Integer shiftLeft(Integer a, Integer b) {
        return a << b;
    }

    @Override
    public Integer shiftRight(Integer a, Integer b) {
        return a >> b;
    }

    @Override
    public Integer shiftRightRight(Integer a, Integer b) {
        return a >>> b;
    }

    @Override
    public Integer negate(Integer exp) {
        if (isChecked && exp == Integer.MIN_VALUE) {
            throw new ArithmeticException("Overflow");
        }
        return -exp;
    }

    @Override
    public Integer l0(Integer res) {
        String bin = Integer.toBinaryString(res);
        return res == 0 ? 32 : 32 - bin.length();
    }

    @Override
    public Integer t0(Integer res) {
        String bin = Integer.toBinaryString(res);
        int ans = 0;
        while (ans < bin.length()) {
            if (bin.charAt(bin.length() - 1 - ans) == '0') {
                ans++;
            } else {
                break;
            }
        }
        return res == 0 ? 32 : ans;
    }

    @Override
    public Integer abs(Integer value) {
        if (isChecked && value == Integer.MIN_VALUE) {
            throw new ArithmeticException("Overflow");
        }
        if (value < 0) {
            return -value;
        }
        return value;
    }

    @Override
    public Integer count(Integer a) {
        return Integer.bitCount(a);
    }

    @Override
    public Integer parseConst(String constant) {
        return Integer.parseInt(constant);
    }
}
