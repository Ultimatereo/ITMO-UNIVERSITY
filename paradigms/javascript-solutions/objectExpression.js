"use strict"

/*
What do u need to do to add a new operation?
1. Create a class for that operation
2. add it to array binaryOperations or unaryOperations
3. add handling in parseBinaryOperation or parseUnaryOperation
 */
function AbstractOperation(operation, evaluate, ...expressions) {
    this.operation = operation;
    this.expressions = expressions;
    this.evaluate = evaluate;
    this.toString = () => this.expressions.map(op => op.toString()).join(" ") + " " + this.operation;
    this.prefix = () => "(" + this.operation + " " + this.expressions.map(op => op.prefix()).join(" ") + ")";
}

const Exception = function (name, message) {
    const result = function (...args) {
        this.message = message(...args);
        this.name = name;
    };
    result.prototype = Object.create(Error.prototype);
    return result;
};
const MissingClosedBracket1 = Exception("MissingClosedBracket",
    (index) => "Brackets are not balanced! There is supposed to be a closed bracket at index: " + index + " !");
const MissingClosedBracket2 = Exception("MissingClosedBracket",
    (index) => "Brackets are not balanced! There is supposed to be a closed bracket for open bracket at index: " + index + " !");
const UnsupportedOperationException = Exception("UnsupportedOperationException",
    (operation, index) => "Operation " + operation + " is unsupported at index: " + index + " !")
const UnsupportedVariableException = Exception("UnsupportedVariableException",
    (variable, index) => "Unsupported variable name: " + variable + " at index: " + index + " !");
const UnknownException = Exception("UnknownException",
    (index) => "There is supposed to be operation, const or variable but it seems like there is nothing like this after index: " + index + " !");
const WrongNumberOfOperandsException = Exception("WrongNumberOfOperandsException",
    (operation, index, gotNumber, expectedNumber) => "There are " + gotNumber + " operands after operation: " + operation +
        " at index: " + index + ". However expected number of operands is " + expectedNumber + ".");

function Add(...expressions) {
    AbstractOperation.apply(this, ["+", (...args) =>
        eval(this.expressions[0].evaluate(...args) + this.expressions[1].evaluate(...args)), ...expressions]);
}

function Subtract(...expressions) {
    AbstractOperation.apply(this, ["-", (...args) =>
        this.expressions[0].evaluate(...args) - this.expressions[1].evaluate(...args), ...expressions]);
}

function Multiply(...expressions) {
    AbstractOperation.apply(this, ["*", (...args) =>
        this.expressions[0].evaluate(...args) * this.expressions[1].evaluate(...args), ...expressions]);
}

function Divide(...expressions) {
    AbstractOperation.apply(this, ["/", (...args) =>
        this.expressions[0].evaluate(...args) / this.expressions[1].evaluate(...args), ...expressions]);
}

function Negate(...expressions) {
    AbstractOperation.apply(this, ["negate", (...args) =>
        -this.expressions[0].evaluate(...args), ...expressions]);
}

function Sinh(...expressions) {
    AbstractOperation.apply(this, ["sinh", (...args) =>
        Math.sinh(this.expressions[0].evaluate(...args)), ...expressions]);
}

function Cosh(...expressions) {
    AbstractOperation.apply(this, ["cosh", (...args) =>
        Math.cosh(this.expressions[0].evaluate(...args)), ...expressions]);
}

function Mean(...expressions) {
    AbstractOperation.apply(this, ["mean", (...args) =>
        this.expressions.map(op => op.evaluate(...args)).reduce((a, b) => a + b, 0) / this.expressions.length, ...expressions]);
}

function Var(...expressions) {
    AbstractOperation.apply(this, ["var", (...args) =>
        this.expressions.map(op => op.evaluate(...args)).reduce((a, b) => a + b*b, 0) / this.expressions.length -
        (this.expressions.map(op => op.evaluate(...args)).reduce((a, b) => a + b, 0) / this.expressions.length)**2,
        ...expressions]);
}

function Const(value) {
    this.value = value;
    this.evaluate = () => eval(this.value);
    this.toString = () => this.value.toString();
    this.prefix = () => this.value.toString();
}

const variables = {"x": 0, "y": 1, "z": 2};

function Variable(variable) {
    this.variable = variable;
    this.evaluate = function (...values) {
        return values[variables[this.variable]];
    }
    this.toString = () => this.variable;
    this.prefix = () => this.variable;
}

const BINARY_OPERATIONS = ['+', '-', '*', '/'];
const UNARY_OPERATIONS = ["negate", "sinh", "cosh"];
const OTHER_OPERATIONS = ["mean", "var"];
const OPERATIONS = BINARY_OPERATIONS.concat(UNARY_OPERATIONS).concat(OTHER_OPERATIONS);

function parseConstOrVariable(v, index) {
    if (!isNaN(v)) {
        return new Const(v);
    } else if (v in variables) {
        return new Variable(v);
    } else {
        throw new UnsupportedVariableException(v, index);
    }
}

function parseBinaryOperation(v, ...expression) {
    switch (v) {
        case '+' :
            return new Add(...expression);
        case '-' :
            return new Subtract(...expression);
        case '*' :
            return new Multiply(...expression);
        case '/' :
            return new Divide(...expression);
    }
}

function parseUnaryOperation(v, ...expression) {
    switch (v) {
        case 'negate' :
            return new Negate(...expression);
        case 'sinh' :
            return new Sinh(...expression);
        case 'cosh' :
            return new Cosh(...expression);
    }
}

function parseOtherOperation(v, ...expression) {
    switch (v) {
        case "mean" :
            return new Mean(...expression);
        case "var" :
            return new Var(...expression);
    }
}

const parse = (str) => {
    // str = splitByWhitespace(str);
    str = str.split(/(\s+)/).filter(e => e.trim().length > 0);
    // \s matches any character that is a whitespace, adding the plus makes it greedy,
    // matching a group starting with characters and ending with whitespace,
    // and the next group starts when there is a character after the whitespace etc.
    let stack = [];
    for (const v of str) {
        if (OPERATIONS.includes(v)) {
            let expression = stack.pop();
            if (BINARY_OPERATIONS.includes(v)) {
                stack.push(parseBinaryOperation(v, stack.pop(), expression));
            } else if (UNARY_OPERATIONS.includes(v)) {
                stack.push(parseUnaryOperation(v, expression));
            } else {
                throw UnsupportedOperationException(v, undefined);
            }
        } else {
            stack.push(parseConstOrVariable(v, undefined));
        }
    }
    return stack.pop();
}
const parsePrefix = (str) => {
    // println("Before: " + str);
    str = str.split(/(\(|\s+|\))/).filter(e => e.trim().length > 0);
    // println("After: " + str);
    return parsePrefixImplementation(str, 0, str.length - 1);
}

function parseSubexpression(startOfTheExpression, start, end, str) {
    if (str[startOfTheExpression] === '(') {
        let balance = 1;
        for (let i = startOfTheExpression + 1; i <= end; i++) {
            if (str[i] === '(') {
                balance++;
            } else if (str[i] === ')') {
                balance--;
            }
            if (balance === 0) {
                return i;
            }
        }
        if (balance !== 0) {
            throw new MissingClosedBracket2(startOfTheExpression);
        }
    } else {
        return startOfTheExpression;
    }
}

const parsePrefixImplementation = (str, start, end) => {
    if (str[start] === '(') { //It's operation
        if (str[end] !== ")") {
            throw new MissingClosedBracket1(end);
        }
        let startOfTheExpression = start + 2;
        let endOfTheExpression = -1;
        // operation [(...)] [(...)] ...
        let expression = [];
        let i = 0;
        while (endOfTheExpression !== end - 1) {
            endOfTheExpression = parseSubexpression(startOfTheExpression, start, end, str);
            expression[i++] = parsePrefixImplementation(str, startOfTheExpression, endOfTheExpression);
            startOfTheExpression = endOfTheExpression + 1;
        }
        if (BINARY_OPERATIONS.includes(str[start + 1])) {
            if (expression.length !== 2) {
                throw new WrongNumberOfOperandsException(str[start + 1], start + 1, expression.length, 2);
            }
            return parseBinaryOperation(str[start + 1], ...expression);
        } else if (UNARY_OPERATIONS.includes(str[start + 1])) {
            if (expression.length !== 1) {
                throw new WrongNumberOfOperandsException(str[start + 1], start + 1, expression.length, 1);
            }
            return parseUnaryOperation(str[start + 1], ...expression);
        } else if (OTHER_OPERATIONS.includes(str[start + 1])) {
            return parseOtherOperation(str[start + 1], ...expression);
        } else {
            throw new UnsupportedOperationException(str[start + 1], start + 1);
        }
    } else if (start === end) {
        return parseConstOrVariable(str[start], start);
    } else {
        throw new UnknownException(start);
    }
}