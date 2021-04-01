package expression;

public class Sqrt implements HelpfulInterface {
    private final HelpfulInterface helpfulInterface;

    public Sqrt(final HelpfulInterface helpfulInterface) {
        this.helpfulInterface = helpfulInterface;
    }

    public int eval(final int value) {
        if (value < 0) {
            throw new ArithmeticException("Square root of negative number");
        }
        int left = 0;
        int right = value;
        while (right - left > 0) {
            int mid = (left + right + 1) / 2;
            if (mid * mid <= value && Integer.MAX_VALUE / mid >= mid) {
                left = mid;
            } else {
                right = mid - 1;
            }
        }
        return left;
    }

    @Override
    public int evaluate(final int num) {
        return eval(helpfulInterface.evaluate(num));
    }

    @Override
    public int evaluate(final int x, final int y, final int z) {
        return eval(helpfulInterface.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "sqrt(" + helpfulInterface.toString() + ")";
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object.getClass() == Sqrt.class) {
            return ((Sqrt) object).helpfulInterface.equals(this.helpfulInterface);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return helpfulInterface.hashCode() * 31;
    }
}
