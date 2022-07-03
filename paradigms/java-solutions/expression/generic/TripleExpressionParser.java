package expression.generic;

import expression.generic.operations.Const;
import expression.generic.operations.Operations;
import expression.generic.operations.Variable;
import expression.generic.operations.binary.*;
import expression.generic.operations.unary.*;

import java.util.*;

import static java.util.Map.entry;

public class TripleExpressionParser<T> extends BaseParser {
    private static final List<String> OPERATIONS = List.of("+", "-", "*", "/", "min", "max", ">>", ">>>", "<<", "t0", "l0", "abs", "//", "**", "count");
    private static final List<Character> STARTS_OF_OPERATIONS = new ArrayList<>();
    private static final Map<String, Integer> PRIORITY = Map.ofEntries(
            entry("//", 3),
            entry("**", 3),
            entry("abs", 10),
            entry("t0", 10),
            entry("l0", 10),
            entry(">>", 0),
            entry(">>>", 0),
            entry("<<", 0),
            entry("min", 0),
            entry("max", 0),
            entry("+", 1),
            entry("-", 1),
            entry("*", 2),
            entry("/", 2),
            entry("(", -1000),
            entry("negate", 10),
            entry("count", 10));
    // before/after     '('     ')'     bin         una      var     const
    // '('              +       +       -           +       +       +
    // ')'              -       +       +           -       -       -
    // bin              +       -       -           +       +       +
    // un               +       -       -           +       +       +
    // var              -       +       +           -       -       -
    // const            -       +       +           -       -       -
    // beginning        +       -       -           +       +       +
    private static final Map<Type, Integer> TYPES_TO_INDEXES = Map.of(
            Type.OP, 0,
            Type.CL, 1,
            Type.BIN, 2,
            Type.UN, 3,
            Type.VAR, 4,
            Type.CONST, 5,
            Type.BEGIN, 6);
    private static final Map<Type, String> TYPES_TO_STRING = Map.of(
            Type.OP, "(",
            Type.CL, ")",
            Type.BIN, "binary operation",
            Type.UN, "unary operation",
            Type.VAR, "variable",
            Type.CONST, "constant",
            Type.BEGIN, "beginning of the string");
    private static final boolean[][] tableOfCoherence = {
            {true, true, false, true, true, true},
            {false, true, true, false, false, false},
            {true, false, false, true, true, true},
            {true, false, false, true, true, true},
            {false, true, true, false, false, false},
            {false, true, true, false, false, false},
            {true, false, false, true, true, true}
    };

    static {
        for (String op : OPERATIONS) {
            STARTS_OF_OPERATIONS.add(op.charAt(0));
        }
    }

    private final Operations<T> opMode;
    boolean taken = false;
    char cur;

    public TripleExpressionParser(String expression, Operations<T> opMode) {
        super(new StringSource(expression));
        this.opMode = opMode;
//            System.err.println("----------------------");
//            System.err.println(expression);
    }

    public TripleExpression<T> parse() {
        Deque<UltimateExpression<T>> stackOfOperands = new ArrayDeque<>();
        Deque<String> stackOfOperations = new ArrayDeque<>();
        boolean mayUnary = true;
        Type before = Type.BEGIN;
        Type after;
        int numOfOpenBrackets = 0;
        while (true) {
//            System.err.println("------------");
//            System.err.println(stackOfOperands);
//            System.err.println(stackOfOperations);
//            System.err.println(cur);
            if (!taken) {
                cur = take();
            }
            if (Character.isWhitespace(cur)) {
                taken = false;
                continue;
            }
            if (cur == '\0') {
                break;
            }
            taken = false;
            if (cur == '(') {
                numOfOpenBrackets++;
                stackOfOperations.push("(");
                mayUnary = true;
                after = Type.OP;
            } else if (cur == ')') {
                if (numOfOpenBrackets < 1) {
                    throw error("There is a closed bracket ). But we missed some open brackets.");
                } else {
                    numOfOpenBrackets--;
                }
//                    System.err.println(stackOfOperations.getFirst());
                while (!stackOfOperations.getFirst().equals("(")) {
                    processOp(stackOfOperands, stackOfOperations.pop());
                }
                stackOfOperations.pop();
                mayUnary = false;
                after = Type.CL;
            } else if (STARTS_OF_OPERATIONS.contains(cur)) {
                String curop = String.valueOf(cur);
                //System.err.println("Operation is here " + cur);
                if (mayUnary && curop.equals("-")) {
                    cur = take();
                    taken = true;
                    if (Character.isDigit(cur)) {
                        StringBuilder sb = new StringBuilder("-");
                        String constant = getNumber(sb);
                        stackOfOperands.push(new Const<>(opMode.parseConst(constant)));
                        mayUnary = false;
                        before = Type.CONST;
                        continue;
                    }
                    curop = "negate";
                    after = Type.UN;
                } else if (curop.equals("m")) {
                    if (take('i')) {
                        expect('n');
                        checkNextChar("min");
                        curop = "min";
                    } else {
                        expect("ax");
                        checkNextChar("max");
                        curop = "max";
                    }
                    after = Type.BIN;
                } else if (curop.equals(">")) {
                    expect('>');
                    if (test('>')) {
                        curop = ">>>";
                        take();
                    } else {
                        curop = ">>";
                    }
                    after = Type.BIN;
                } else if (curop.equals("<")) {
                    expect('<');
                    curop = "<<";
                    after = Type.BIN;
                } else if (curop.equals("t")) {
                    expect('0');
                    checkNextChar("t0");
                    curop = "t0";
                    after = Type.UN;
                } else if (curop.equals("l")) {
                    expect('0');
                    checkNextChar("l0");
                    curop = "l0";
                    after = Type.UN;
                } else if (curop.equals("a")) {
                    expect("bs");
                    checkNextChar("abs");
                    curop = "abs";
                    after = Type.UN;
                } else if (curop.equals("c")) {
                    expect("ount");
                    checkNextChar("count");
                    curop = "count";
                    after = Type.UN;
                } else if (curop.equals("/")) {
                    if (take('/')) {
                        curop = "//";
                    }
                    after = Type.BIN;
                } else if (curop.equals("*")) {
                    if (take('*')) {
                        curop = "**";
                    }
                    after = Type.BIN;
                } else {
                    after = Type.BIN;
                }
                //System.err.println("curop " + curop);
                while (!stackOfOperations.isEmpty() &&
                        ((!checkUnary(curop) &&
                                PRIORITY.get(stackOfOperations.getFirst()) >=
                                        PRIORITY.get(curop)) ||
                                (checkUnary(curop) &&
                                        PRIORITY.get(stackOfOperations.getFirst()) >
                                                PRIORITY.get(curop)))) {
                    processOp(stackOfOperands, stackOfOperations.pop());
                }
                stackOfOperations.push(curop);
                mayUnary = true;
            } else if (Character.isLetter(cur)) {
                StringBuilder sb = new StringBuilder(cur);
                String var = getVar(sb);
                //System.err.println(var);
                if (var.equals("x") || var.equals("y") || var.equals("z")) {
                    stackOfOperands.push(new Variable<>(var));
                    mayUnary = false;
                    after = Type.VAR;
                } else {
                    throw error("Unsupported variable: " + var);
                }
            } else if (Character.isDigit(cur)) {
                StringBuilder sb = new StringBuilder();
                String constant = getNumber(sb);
                stackOfOperands.push(new Const<>(opMode.parseConst(constant)));
                mayUnary = false;
                after = Type.CONST;
            } else {
                throw error("Unknown symbol: " + cur);
            }
            //System.err.println(before + " " + after);
            if (!tableOfCoherence[TYPES_TO_INDEXES.get(before)][TYPES_TO_INDEXES.get(after)]) {
                throw error("After " + TYPES_TO_STRING.get(before) + " can't be " + TYPES_TO_STRING.get(after) + "!");
            } else {
                before = after;
            }
        }
        if (numOfOpenBrackets > 0) {
            throw error("Some brackets are still not closed!");
        }
        while (!stackOfOperations.isEmpty()) {
            processOp(stackOfOperands, stackOfOperations.pop());
        }
        if (stackOfOperands.size() == 0) {
            throw error("There are no constants or variables!");
        }
        return stackOfOperands.pop();
    }

    private boolean checkUnary(String curop) {
        return curop.equals("negate") || curop.equals("l0") || curop.equals("t0");
    }

    private void checkNextChar(String op) {
        cur = take();
        taken = true;
        if (Character.isDigit(cur) || Character.isLetter(cur)) {
            throw error("You must not use letters or digits after " + op + " to avoid ambiguous situation! actual: " + cur);
        }
    }

    private String getVar(StringBuilder sb) {
        while (Character.isLetter(cur) || cur == '_' || Character.isDigit(cur)) {
            sb.append(cur);
            cur = take();
        }
        taken = true;
        return sb.toString();
    }


    private String getNumber(StringBuilder sb) {
        while (Character.isDigit(cur)) {
            sb.append(cur);
            cur = take();
        }
        taken = true;
        return sb.toString();
    }

    private void processOp(Deque<UltimateExpression<T>> stackOfOperands, String op) {
        if (stackOfOperands.isEmpty()) {
            throw error("There are no arguments for operation: " + op);
        }
        UltimateExpression<T> r = stackOfOperands.pop();
        if (op.equals("negate") || op.equals("t0") || op.equals("l0") || op.equals("abs") || op.equals("count")) {
            switch (op) {
                case "negate" -> stackOfOperands.push(new Negate<>(r, opMode));
                case "t0" -> stackOfOperands.push(new T0<>(r, opMode));
                case "l0" -> stackOfOperands.push(new L0<>(r, opMode));
                case "abs" -> stackOfOperands.push(new Abs<>(r, opMode));
                case "count" -> stackOfOperands.push(new Count<>(r, opMode));
            }
        } else {
            if (stackOfOperands.isEmpty()) {
                throw error("There is only one argument for operation: " + op + ". But needed 2.");
            }
            UltimateExpression<T> l = stackOfOperands.pop();
//                System.err.println("op: l and r: " + l + " " + r);
//                System.err.println("operation: " + op);
            switch (op) {
                case "+" -> stackOfOperands.push(new Add<>(l, r, opMode));
                case "-" -> stackOfOperands.push(new Subtract<>(l, r, opMode));
                case "*" -> stackOfOperands.push(new Multiply<>(l, r, opMode));
                case "/" -> stackOfOperands.push(new Divide<>(l, r, opMode));
                case "min" -> stackOfOperands.push(new Min<>(l, r, opMode));
                case "max" -> stackOfOperands.push(new Max<>(l, r, opMode));
                case ">>" -> stackOfOperands.push(new ShiftRight<>(l, r, opMode));
                case ">>>" -> stackOfOperands.push(new ShiftRightRight<>(l, r, opMode));
                case "<<" -> stackOfOperands.push(new ShiftLeft<>(l, r, opMode));
                case "**" -> stackOfOperands.push(new Pow<>(l, r, opMode));
                case "//" -> stackOfOperands.push(new Log<>(l, r, opMode));
                default -> throw new AssertionError("Unknown operation appeared: " + op);
            }
        }
    }
}
