import { SyntaxError } from "@tvm/lab03";
import { test } from "@tvm/mark";
import { parseCompileAndExecute } from "../src";

describe('testing addition and multiplication regression', () => {
    test("Empty expression is invalid", 3, parseCompileAndExecute, SyntaxError, "");
    test("Number is a valid expression", 3, parseCompileAndExecute, 42, "42");
    test("Addition is a valid expression", 3, parseCompileAndExecute, 42, "35+7");
    test("Spaces are permitted in the addition", 4, parseCompileAndExecute, 42, "  35 +7");
    test("Tabs are permitted in the addition", 4, parseCompileAndExecute, 42, "7\t+35");
    test("Newlines are permitted in the addition", 4, parseCompileAndExecute, 42, `
        35  +
        7`);
    test("Multiple additions are valid", 3, parseCompileAndExecute, 42, "5+4+3+2+1+2+3+4+5+6+7");
    test("Multiplication is a valid expression", 3, parseCompileAndExecute, 42, "6*7");
    test("Multiplication is performed before addition (1)", 4, parseCompileAndExecute, 42, "(6*6)+6");
    test("Multiplication is performed before addition (2)", 3, parseCompileAndExecute, 42, "7+5*7");
    test("Multiplication is performed before addition (3)", 4, parseCompileAndExecute, 72, "6*(6+6)");
    test("Multiplication is performed before addition (4)", 4, parseCompileAndExecute, 84, "(7+5)*7");
    test("Complex expressions are supported", 4, parseCompileAndExecute, 42, "7+2*7+3*6+3");
    test("Parentheses are correctly supported", 4, parseCompileAndExecute, 42, "(2+1)*7+(3*2*3)+2*(1+0)+1");
    test("Extra paren are ok", 4, parseCompileAndExecute, 42, "(2+1)*7+(3*(2)*3)+2*((1+(0)))+1");
});

describe('testing subtraction and division', () => {
    test("Subtraction is supported", 3, parseCompileAndExecute, 21, "42-21");
    test("Subtraction is left-associative", 4, parseCompileAndExecute, 5, "10-4-1");
    test("Associativity is preserved across addition and subtraction", 4, parseCompileAndExecute, 3, "5+2-4");
    test("Associativity can be overriden via parentheses (1)", 4, parseCompileAndExecute, 5, "5+(2-4)+(3-1)");
    test("Associativity can be overriden via parentheses (2)", 4, parseCompileAndExecute, -1, "(5+2)-(4+3)-1");
    
    test("Division is supported", 3, parseCompileAndExecute, 2, "42/21");
    test("Division is left-associative", 4, parseCompileAndExecute, 1, "8/4/2");
    test("Division by zero gets runtime error", 3, parseCompileAndExecute, WebAssembly.RuntimeError, "1/0");
});
describe('testing unary negation', () => {
    test("unary minus is supported", 3, parseCompileAndExecute, 42, "43+-1");
    test("double unary minus is supported", 3, parseCompileAndExecute, 42, "41--1");
});
describe('testing variables', () => {
    test("variables can be used", 3, parseCompileAndExecute, 42, "x+1", 41);
    test("undefined variables yield RuntimeError", 4, parseCompileAndExecute, WebAssembly.RuntimeError, "x+y");
    test("Dividing undefined by zero yields RunTimeError", 3, parseCompileAndExecute, WebAssembly.RuntimeError, "x/0");
    test("complex expressions are supported", 4, parseCompileAndExecute, 42, "(x*y+1)*y*x", 3, 2);
    test("Dividing defined by zero gets runtime error", 3, parseCompileAndExecute, WebAssembly.RuntimeError, "1/x", 0);
});
