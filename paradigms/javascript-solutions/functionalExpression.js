"use strict"
const abstractOperation = (f) =>
    (...expressions) =>
        (...values) => {
            let valuesOfExpressions = [];
            for (let i = 0; i < expressions.length; i++) {
                valuesOfExpressions[i] = expressions[i](...values);
            }
            return f(...valuesOfExpressions);
        }
const add = abstractOperation((a, b) => eval(a + b));
const subtract = abstractOperation((a, b) => a - b);
const multiply = abstractOperation((a, b) => a * b);
const divide = abstractOperation((a, b) => a / b);
const negate = abstractOperation((a) => -a);
const sinh = abstractOperation((a) => Math.sinh(a));
const cosh = abstractOperation((a) => Math.cosh(a));
const cnst = (name) => () => eval(name);
const pi = cnst(Math.PI);
const e = cnst(Math.E);
const variable = (name) =>
    (...values) => {
        switch (name) {
            case "x" : return values[0];
            case "y" : return values[1];
            case "z" : return values[2];
            default : return null;
        }
    }
