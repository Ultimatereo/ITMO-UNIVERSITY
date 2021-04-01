package expression;

public class Abs implements HelpfulInterface {
    private final HelpfulInterface helpfulInterface;

    public Abs(final HelpfulInterface helpfulInterface) {
        this.helpfulInterface = helpfulInterface;
    }

    public int eval(final int a) {
        if (a == Integer.MIN_VALUE) {
            throw new ArithmeticException("Overflow");
        }
        if (a > 0) {
            return a;
        } else {
            return -a;
        }
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
        return "abs(" + helpfulInterface.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Abs.class) {
            return ((Abs) obj).helpfulInterface.equals(this.helpfulInterface);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return helpfulInterface.hashCode() * 31;
    }
}

