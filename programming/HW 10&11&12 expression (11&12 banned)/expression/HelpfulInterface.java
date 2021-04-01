package expression;

public interface HelpfulInterface extends Expression, TripleExpression {
    @Override
    int evaluate(int value);

    @Override
    int evaluate(int value1, int value2, int value3);

}
