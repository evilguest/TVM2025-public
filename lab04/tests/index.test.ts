import { test } from "../../mark";
import { parseAndPrint } from "../src";

describe('testing addition and multiplication printing', () => {

    test("Number prints to itself", 3, parseAndPrint, "42", "42");
    test("Addition is properly spaced", 3, parseAndPrint, "35 + 7", "35+7");
    test("Extra spaces are removed", 4, parseAndPrint, "35 + 7", "  35 +7");
    test("Tabs are removed from the addition", 4, parseAndPrint, "7 + 35", "7\t+35");
    test("Newlines are removed from the addition", 4, parseAndPrint, "35 + 7", `
        35  +
        7`);
    test("Parentheses are removed from addition", 4, parseAndPrint, "5 + 4 + 3 + 2 + 1 + 2 + 3 + 4 + 5 + 6 + 7", "5+((4+3)+2) +1+ (2+3 + 4)+5+6+7" );
    test("Multiplication is properly spaced", 3, parseAndPrint, "6 * 7", "6*7");
    test("Multiplication is performed before addition (1)", 4, parseAndPrint, "6 * 6 + 6", "(6*6)+6");
    test("Multiplication is performed before addition (2)", 3, parseAndPrint, "7 + 5 * 7", "7+5*7");
    test("Multiplication is performed before addition (3)", 4, parseAndPrint, "6 * (6 + 6)", "6*(6+6)");
    test("Multiplication is performed before addition (4)", 4, parseAndPrint, "(7 + 5) * 7", "(7+5)*7");
    test("Complex expressions are supported", 3, parseAndPrint, "7 + 2 + 7 + 3 * 6 * 3", "7+2+7+3*6*3");  
    test("Complex expressions with paren are supported", 4, parseAndPrint, "7 + 2 * 7 + 3 * 6 + 3", "7+2*7+3*6+3");
    test("Parentheses are correctly supported", 4, parseAndPrint, "(2 + 1) * 7 + 3 * 2 * 3 + 2 * (1 + 0) + 1", "(2+1)*7+(3*2*3)+2*(1+0)+1");
    test("Extra paren are removed", 4, parseAndPrint, "(2 + 1) * 7 + 3 * 2 * 3 + 2 * (1 + 0) + 1", "(2+1)*7+(3*(2)*3)+2*((1+(0)))+1");
    });
describe('Testing subtraction and division', () => {
    test("Subtraction is supported", 3, parseAndPrint, "42 - 21", "42-21");
    test("Subtraction is left-associative", 4, parseAndPrint, "10 - 4 - 1", "10-4-1");
    test("Associativity is preserved across addition and subtraction", 4, parseAndPrint, "5 - 2 - 4", "5-2-4");
    test("Associativity can be overriden via parentheses 1", 4, parseAndPrint, "5 + 2 - (4 + 3) - 1", "(5+2)-(4+3)-1");
    test("Associativity can be overriden via parentheses 2", 4, parseAndPrint, "5 - (4 - 3)", "5-(4-3)");
    test("Extra parenthesis are removed in subtraction", 4, parseAndPrint, "8 - (4 + 2)", "8-(((4+2)))");
    test("Division is supported", 3, parseAndPrint, "42 / 21", "42/21");
    test("Division is left-associative", 4, parseAndPrint, "8 / 4 / 2", "8/4/2");
    test("Division associativity can be overriden 1", 4, parseAndPrint, "8 / (4 / 2)", "8/(4/2)");
    test("Division associativity can be overriden 2", 4, parseAndPrint, "8 / (4 * 2)", "8/(4*2)");
    test("Extra parenthesis are removed", 4, parseAndPrint, "8 / (4 / 2)", "8/(((4/2)))");
});
describe('Testing unary negation', () => {
    test("Unary minus is supported in addition", 3, parseAndPrint, "43 + -1", "43+-1");
    test("Unary minus in addition is properly parenthesized 1", 4, parseAndPrint, "43 + -1", "(43)+-(1)");
    test("Unary minus in addition is properly parenthesized 2", 4, parseAndPrint, "43 + -1", "(43)+(-1)");
    test("Unary minus is supported in subtraction", 3, parseAndPrint, "43 - -1", "43--1");
    test("Unary minus in subtraction is properly parenthesized 1", 4, parseAndPrint, "43 - -1", "(43)--(1)");
    test("Unary minus in subtraction is properly parenthesized 2", 4, parseAndPrint, "43 - -1", "(43)-(-1)");
});
describe('testing variables', () => {
    test("variables can be used", 3, parseAndPrint, "x + 1", "x+1");
    test("Two variables are handled", 3, parseAndPrint, "x + y", "x+y");
    test("Complex expressions are properly printed",  4, parseAndPrint, "(x * y + 1) * y * x", "(x*y+1)*y*x");
});
