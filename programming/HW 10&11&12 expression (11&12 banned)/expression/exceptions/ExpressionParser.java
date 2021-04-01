package expression.exceptions;

import expression.*;
import expression.parser.BaseParser;
import expression.parser.StringSource;

public class ExpressionParser extends BaseParser implements Parser {
    public HelpfulInterface parse(String expression) {
        this.source = new StringSource(expression);
        nextChar();
        HelpfulInterface result = parseExpression();
        if (eof()) {
            return result;
        }

        throw error("Expected end of input, actual: '" + ch + '\'');
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
            HelpfulInterface lowestLevel = parseExpression();
            expect(')');
            return lowestLevel;
        } else if (ch == '-') {
            return parseUnaryMinus();
        } else if (isLetter()) {
            String name = readName();
            if (name.equals("sqrt")) {
                return new Sqrt(parseFundamental());
            } else if (name.equals("abs")) {
                return new Abs(parseFundamental());
            }
            return new Variable(name);
        }

        throw error("Expected constant, variable or parentheses, actual: '" + ch + '\'');
    }

    private HelpfulInterface parseUnaryMinus() {

        expect('-');

        skipWhiteSpace();
        if (isDigit()) {
            return parseNegativeConst();
        } else if (test('(')) {
            HelpfulInterface nestedHelpfulInterface = parseExpression();
            expect(')');
            return new CheckedNegate(nestedHelpfulInterface);
        } else if (isLetter()) {
            String name = readName();
            if (name.equals("abs")) {
                return new CheckedNegate(new Abs(parseFundamental()));
            } else if (name.equals("sqrt")) {
                return new CheckedNegate(new Sqrt(parseFundamental()));
            }
            return new CheckedNegate(new Variable(name));
        }
        return new CheckedNegate(parseUnaryMinus());
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
                case '+': left = new CheckedAdd(left, right);
                    break;
                case '-': left = new CheckedSubtract(left, right);
                    break;
                case '*': left = new CheckedMultiply(left, right);
                    break;
                case '/': left = new CheckedDivide(left, right);
                    break;
            }
        }

        return left;
    }

    private HelpfulInterface parseConst() {
        String value = readDigits();
        return new Const(Integer.parseInt(value));
    }

    private HelpfulInterface parseNegativeConst() {
        String value = readDigits();
        return new Const(Integer.parseInt('-' + value));
    }


    private void skipWhiteSpace() {
        while (Character.isWhitespace(ch)) {
            nextChar();
        }
    }

    private String readName() {
        final StringBuilder sb = new StringBuilder();

        do {
            sb.append(ch);
            nextChar();
        } while (isLetter());

        String s = sb.toString();
        if (s.equals("x") || s.equals("y") || s.equals("z") || s.equals("abs") || s.equals("sqrt"))
            return s;

        throw error("Unexpected variable or function name: " + s);
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
}