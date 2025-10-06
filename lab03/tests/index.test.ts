import { test } from '../../mark';
import { evaluate, SyntaxError } from '../src';
import grammar from '../src/arith.ohm-bundle';
import { findLeftRecursion } from './recursionChecker';

describe('testing addition and multiplication regression', () => {
    test("Empty expression is invalid", 3, evaluate, SyntaxError, "");
    test("Number is a valid expression", 3, evaluate, 42, "42");
    test("Numbers with spaces are invalid", 3, evaluate, SyntaxError, "42 7");
    test("Addition is a valid expression", 3, evaluate, 42, "35+7");
    test("Incomplete addition is invalid", 3, evaluate, SyntaxError, "42+");
    test("Standalone + is invalid", 3, evaluate, SyntaxError, "+42");
    test("Sequential pluses are invalid", 3, evaluate, SyntaxError, "7++35");
    test("Spaces are permitted in the addition", 4, evaluate, 42, "  35 +7");
    test("Tabs are permitted in the addition", 4, evaluate, 42, "7\t+35");
    test("Newlines are permitted in the addition", 4, evaluate, 42, `
        35  +
        7`);
    test("Multiple additions are valid", 3, evaluate, 42, "5+4+3+2+1+2+3+4+5+6+7");
    
    test("Multiplication is a valid expression", 3, evaluate, 42, "6*7");
    test("Incomplete multiplication is invalid", 3, evaluate, SyntaxError, "42*");
    test("Standalone * is invalid", 3, evaluate, SyntaxError, "*42");
    test("Multiplication is performed before addition", 4, evaluate, 42, "7+4*7+7");

    test("Complex expressions are supported", 4, evaluate, 42, "7+2*7+3*6+3");
    test("Parentheses are correctly supported", 4, evaluate, 42, "(2+1)*7+(3*2*3)+2*(1+0)+1");
    test("Extra paren are ok", 4, evaluate, 42, "(2+1)*7+(3*(2)*3)+2*((1+(0)))+1");
 
});
describe('testing subtraction and division', () => {
    test("Subtraction is supported", 3, evaluate, 21, "42-21");
    test("Subtraction is left-associative", 4, evaluate, 5, "10-4-1");
    test("Associativity is preserved across addition and subtraction", 4, evaluate, 3, "5+2-4");
    test("Associativity can be overriden via parentheses", 4, evaluate, 5, "5+(2-4)+(3-1)");
    test("Division is supported", 3, evaluate, 2, "42/21");
    test("Division is performed before addition", 4, evaluate, 3, "1 + 42/21");
    test("Multiplication is performed before subtraction", 4, evaluate, 0, "42 - 6*7");
    test("Division is left-associative", 4, evaluate, 1, "8/4/2");
    test("Division by zero yields Error", 3, evaluate, Error, "8/0");
});
describe('testing unary negation', () => {
    test("unary minus is supported", 3, evaluate, 42, "43+-1");
    test("double unary minus is supported", 4, evaluate, 42, "41--1");
    test("Unary minus supports spaces properly", 4, evaluate, 42, "43 - - -1");
});
describe('testing variables', () => {
    test("variables can be used", 3, evaluate, 42, "x+1", {x: 41});
    test("undefined variables yield NaN", 3, evaluate, NaN, "x+y", {x: 41});
    test("Dividing undefined by zero yields Error", 3, evaluate, Error, "x/0");

});

describe('testing left-associativity of the grammar', () => {
   test("No left-recursive rules", 5, findLeftRecursion, undefined, grammar);
});
