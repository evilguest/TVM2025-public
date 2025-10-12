import { ReversePolishNotationActionDict} from "./rpn.ohm-bundle";

export const rpnCalc = {
    number(arg0) {
        return parseInt(arg0.sourceString);
    },
    Exp_sum(arg0 : any, arg1 : any, _ : any) {
        return arg0.calculate() + arg1.calculate();
    },
    Exp_mul(arg0 : any, arg1 : any, _ : any) {
        return arg0.calculate() * arg1.calculate();
    }
} satisfies ReversePolishNotationActionDict<number>;
