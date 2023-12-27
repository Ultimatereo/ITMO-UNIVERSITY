package expression.exceptions;

import expression.TripleExpression;
import expression.parser.TripleExpressionParser;

public class ExpressionParser implements Parser {

    @Override
    public TripleExpression parse(String expression) throws Exception {
        return new TripleExpressionParser(expression).parse(true);
    }
}
