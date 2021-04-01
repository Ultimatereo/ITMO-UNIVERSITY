package expression;

public class UnaryMinus implements HelpfulInterface {
    private final HelpfulInterface helpfulInterface;

    public UnaryMinus(final HelpfulInterface helpfulInterface) {
        this.helpfulInterface = helpfulInterface;
    }

    public int eval(final int value) {
        return -value;
    }

    @Override
    public int evaluate(final int value) {
        return eval(helpfulInterface.evaluate(value));
    }

    @Override
    public int evaluate(final int value1, final int value2, final int value3) {
        return eval(helpfulInterface.evaluate(value1, value2, value3));
    }

    @Override
    public String toString() {
        return "-" + helpfulInterface.toString();
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object.getClass() == UnaryMinus.class) {
            return ((UnaryMinus) object).helpfulInterface.equals(this.helpfulInterface);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return helpfulInterface.hashCode() * 31;
    }
}

