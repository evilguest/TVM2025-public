import { test } from "../../mark";
import { Expr, parseExpr } from "../../lab04";

import { simplify, cost } from "../src";
import { basicIdentities, commutativeIdentities, toughIdentities } from './identities';


const estimate = (source: string) => cost(parseExpr(source));

describe('Testing cost function', ()=>
{
    test("const cost is zero", 3, estimate, 0, "42");
    test("var cost is one", 3, estimate, 1, "x");
    test("unary minus cost is one", 3, estimate, 2, "-x");
    test("addition cost is 1 a", 3, estimate, 1, "42+1");
    test("addition cost is 1 b", 3, estimate, 2, "42+x");
    test("addition cost is 1 c", 3, estimate, 4, "x+42+y");

    test("multiplication cost is 1 a", 3, estimate, 1, "42*1");
    test("multiplicationcost is 1 b", 3, estimate, 2, "42*x");
    test("multiplication cost is 1 c", 3, estimate, 4, "x*42*y");

    test("subtraction cost is 1 a", 3, estimate, 1, "42-1");
    test("subtraction is 1 b", 3, estimate, 2, "42-x");
    test("subtraction cost is 1 c", 3, estimate, 4, "x-42-y");

    test("complex expressions are estimated properly", 4, estimate, 6, "(x+y)*(a+1)");


});

const parseSimplifyAndCost = (source: string, identities: ()=>[Expr, Expr][]) => cost(simplify(parseExpr(source), identities()));

describe('Testing simplify function', ()=>
{
    test("42+0 => 42", 3, parseSimplifyAndCost, 0, "42+0", basicIdentities);
    test("x+0 => x", 3, parseSimplifyAndCost, 1, "a+0", basicIdentities);
    test("x*0 => 0", 3, parseSimplifyAndCost, 0, "a*0", basicIdentities);
    test("x*0*0 => 0", 3, parseSimplifyAndCost, 0, "a*0*0", basicIdentities);
    test("x-0 => x", 3, parseSimplifyAndCost, 1, "x-0", basicIdentities);
    test("x+0-0 => x", 3, parseSimplifyAndCost, 1, "x+0-0", basicIdentities);

    test("x*(1-1)=>0", 4, parseSimplifyAndCost, 0, "a*(1-1)", basicIdentities);

    test("--42=>42", 4, parseSimplifyAndCost, 0, "--42", commutativeIdentities)
    test("x*0*y=>0", 4, parseSimplifyAndCost, 0, "x*0*y", commutativeIdentities);
    test("x*(1+0*y)=>x", 4, parseSimplifyAndCost, 1, "x*(1+0*y)", commutativeIdentities);
    test("(a+b)*(b+a)-(a+b)*(a+b)=>4*a*b", 5, parseSimplifyAndCost, 0, "(a+b)*(b+a)-(b+a)*(a+b)", toughIdentities);
});

