package expression.parser;

import expression.*;

public class ExpressionParser extends BaseParser implements Parser {
    public HelpfulInterface parse(String expression) {
        this.source = new StringSource(expression);
        nextChar();
        HelpfulInterface result = parseLogic();
        if (eof()) {
            return result;
        }

        throw error("Expected end of input, actual: '" + ch + '\'');
    }

    private HelpfulInterface parseLogic() {
        return parseLogicOperation('|');
    }

    private HelpfulInterface parseExpression() {
        return parseBinaryOperation('+', '-');
    }

    private HelpfulInterface parseTerm() {
        return parseBinaryOperation('*', '/');
    }

    private HelpfulInterface parseFundamental() {
        skipWhiteSpace();

        if (isDigit()) {
            return parseConst();
        } else if (test('(')) {
            HelpfulInterface lowestLevel = parseLogic();
            expect(')');
            return lowestLevel;
        } else if (ch == '-') {
            return parseUnaryMinus();
        } else if (isLetter()) {
            return new Variable(readVariable());
        }

        throw error("Expected constant, variable or parentheses, actual: '" + ch +'\'');
    }

    private HelpfulInterface parseUnaryMinus() {

        expect('-');

        skipWhiteSpace();
        if (isDigit()) {
            return parseNegativeConst();
        } else if (test('(')) {
            HelpfulInterface nestedHelpfulInterface = parseLogic();
            expect(')');
            return new UnaryMinus(nestedHelpfulInterface);
        } else if (isLetter()) {
            return new UnaryMinus(new Variable(readVariable()));
        }
        return new UnaryMinus(parseUnaryMinus());
    }

    private HelpfulInterface parseBinaryOperation(char firstOperation, char secondOperation) {
        HelpfulInterface left;
        if (firstOperation == '+') {
            left = parseTerm();
        } else {
            left = parseFundamental();
        }

        while (!test(END)) {
            skipWhiteSpace();
            char op = ch;
            if (op != firstOperation && op != secondOperation) {
                break;
            }
            nextChar();

            HelpfulInterface right;
            if (firstOperation == '+') {
                right = parseTerm();
            } else {
                right = parseFundamental();
            }
            switch(op) {
                case '+': left = new Add(left, right);
                    break;
                case '-': left = new Subtract(left, right);
                    break;
                case '*': left = new Multiply(left, right);
                    break;
                case '/': left = new Divide(left, right);
                    break;
            }
        }
        return left;
    }

    private HelpfulInterface parseLogicOperation(final char operation) {
        HelpfulInterface left = getLogicLowerOperation(operation);

        while (!test(END)) {
            skipWhiteSpace();
            char op = ch;
            if (op != operation) {
                break;
            }
            nextChar();

            HelpfulInterface right = getLogicLowerOperation(operation);
            switch (op) {
                case '|': left = new Or(left, right);
                    break;
                case '&': left = new And(left, right);
                    break;
                case '^': left = new Xor(left, right);
                    break;
            }
        }

        return left;
    }

    private HelpfulInterface getLogicLowerOperation(final char operation) {
        switch(operation) {
            case '|' : return parseLogicOperation('^');
            case '^' : return parseLogicOperation('&');
            default: return parseExpression();
        }
    }

    private HelpfulInterface parseConst() {
        String value = readDigits();
        return new Const(Integer.parseInt(value));
    }

    private HelpfulInterface parseNegativeConst() {
        String value = readDigits();
        return new Const(Integer.parseInt('-' + value));
    }

    private String readVariable() {
        final StringBuilder sb = new StringBuilder();

        do {
            sb.append(ch);
            nextChar();
        } while (isLetter());

        String s = sb.toString();
        if (s.equals("x") || s.equals("y") || s.equals("z"))
            return s;

        throw error("Unexpected variable name: " + s);
    }

    private String readDigits() {
        final StringBuilder sb = new StringBuilder();

        do {
            sb.append(ch);
            nextChar();
        } while (isDigit());

        return sb.toString();
    }

    private boolean isLetter() {
        return between('a', 'z') || between('A', 'Z');
    }

    private boolean isDigit() {
        return between('0', '9');
    }
    private void skipWhiteSpace() {
        while (Character.isWhitespace(ch)) {
            nextChar();
        }
    }
}