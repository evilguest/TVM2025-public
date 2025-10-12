import { ReversePolishNotationActionDict } from "./rpn.ohm-bundle";

export type StackDepth = { max: number, out: number };

export const rpnStackDepth = {
    number(arg0: any): StackDepth {
        return {max : 1, out : 1};
    },
    Exp_sum(arg0: any, arg1 : any, arg2 : any) {
        const stackA = arg0.stackDepth;
        const stackB = arg1.stackDepth
        const res = stackA.out + stackB.out - 1;
        const mx = Math.max(stackA.max, stackA.out + stackB.max);
        return {max : mx, out : res};
    },
    Exp_mul(arg0: any, arg1 : any, arg2 : any) {
        const stackA = arg0.stackDepth;
        const stackB = arg1.stackDepth
        const res = stackA.out + stackB.out - 1;
        const mx = Math.max(stackA.max, stackA.out + stackB.max);
        return {max : mx, out : res};
    },
} satisfies ReversePolishNotationActionDict<StackDepth>;