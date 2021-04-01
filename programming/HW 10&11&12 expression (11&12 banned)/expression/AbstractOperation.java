package expression;

import java.util.Objects;

public abstract class AbstractOperation implements HelpfulInterface {
    private HelpfulInterface helpfulInterface1;
    private HelpfulInterface helpfulInterface2;
    private String operation;

    public AbstractOperation(HelpfulInterface x, HelpfulInterface y, String operation) {
        this.helpfulInterface1 = x;
        this.helpfulInterface2 = y;
        this.operation = operation;
    }

    abstract protected int eval(int value1, int value2);

    @Override
    public int evaluate(int value) {
        return eval(helpfulInterface1.evaluate(value), helpfulInterface2.evaluate(value));
    }
    @Override
    public int evaluate(int value1, int value2, int value3) {
        return eval(helpfulInterface1.evaluate(value1, value2, value3), helpfulInterface2.evaluate(value1, value2, value3));
    }
    @Override
    public String toString() {
        return "(" + helpfulInterface1 + " " + operation + " " + helpfulInterface2 + ")";
    }
    @Override
    public boolean equals(Object object) {
        if (object != null && object.getClass() == getClass()) {
            AbstractOperation that = (AbstractOperation) object;
            return helpfulInterface1.equals(that.helpfulInterface1) && helpfulInterface2.equals(that.helpfulInterface2);
        }
        return false;
    }

    @Override
    final public int hashCode() {
        return Objects.hash(helpfulInterface1, helpfulInterface2, operation);
    }
}