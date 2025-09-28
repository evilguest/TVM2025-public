import { Dict, MatchResult, Semantics } from "ohm-js";
import grammar, { AddMulActionDict } from "./addmul.ohm-bundle";

export const addMulSemantics: AddMulSemantics = grammar.createSemantics() as AddMulSemantics;

const addMulCalc = {
    number(arg0) {
        return parseInt(arg0.sourceString)
    },
    Exp: function(arg: any) {
        return arg.calculate();
    },
    AddExp_plus: function(arg0: any, arg1: any, arg2: any) {
        return arg0.calculate() + arg2.calculate();
    },
    MulExp_mul: function(arg0: any, arg1: any, arg2: any) {
        return arg0.calculate() * arg2.calculate();
    },
    BrExp_braces: function(arg0: any, arg1: any, arg2: any) {
        return arg1.calculate();
    }
} satisfies AddMulActionDict<number>

addMulSemantics.addOperation<Number>("calculate()", addMulCalc);

interface AddMulDict extends Dict {
    calculate(): number;
}

interface AddMulSemantics extends Semantics
{
    (match: MatchResult): AddMulDict;
}