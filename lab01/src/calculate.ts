import { Dict, MatchResult, Semantics } from "ohm-js";
import grammar, { AddMulActionDict } from "./addmul.ohm-bundle";

export const addMulSemantics: AddMulSemantics = grammar.createSemantics() as AddMulSemantics;


const addMulCalc = {
    number: function(_: any) {
        return parseInt(this.sourceString);
    },
    Exp: function(e: any) {
        return e.calculate();
    },
    AddExp_plus: function(a: any, _: any, b: any) {
        return a.calculate() + b.calculate();
    },
    MulExp_times: function(a: any, _: any, b: any) {
        return a.calculate() * b.calculate();
    },
    BrExp_paren: function(_1: any, e: any, _2: any) {
        return e.calculate();
    }
} satisfies AddMulActionDict<number>

addMulSemantics.addOperation<Number>("calculate()", addMulCalc);

interface AddMulDict  extends Dict {
    calculate(): number;
}

interface AddMulSemantics extends Semantics
{
    (match: MatchResult): AddMulDict;
}
