import { Dict, MatchResult, Semantics } from "ohm-js";
import grammar, { AddMulActionDict } from "./addmul.ohm-bundle";

export const addMulSemantics: AddMulSemantics = grammar.createSemantics() as AddMulSemantics;


const addMulCalc = {
    number(arg0) {
        return parseInt(arg0.sourceString)
    },
    AddExp_plus(arg0 : any, arg1, arg2 : any) {
        return arg0.calculate() + arg2.calculate()
    },
    MulExp_mult(arg0 : any, arg1, arg2 : any) {
        return arg0.calculate() * arg2.calculate()
    },
    AtomExp_braces(arg0, arg1 : any, arg2) {
        return arg1.calculate()
    },
} satisfies AddMulActionDict<number>

addMulSemantics.addOperation<Number>("calculate()", addMulCalc);

interface AddMulDict  extends Dict {
    calculate(): number;
}

interface AddMulSemantics extends Semantics
{
    (match: MatchResult): AddMulDict;
}
