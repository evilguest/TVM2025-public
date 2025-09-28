import { MatchResult } from "ohm-js";
import grammar, { ArithmeticActionDict, ArithmeticSemantics } from "./arith.ohm-bundle";

export const arithSemantics: ArithSemantics = grammar.createSemantics() as ArithSemantics;


const arithCalc = {
    Program_main(_l: any, e : any, _r: any, _end: any)   
     {
        return e.calculate(this.args.params)
    },


    AddExp_iter(head: any, _ops: any, _rights: any) {
        let acc = head.calculate(this.args.params);
        const n = _rights.children.length;           
        for (let i = 0; i < n; i++) {
            const op  = _ops.children[i].sourceString; 
            const rhs = _rights.child(i).calculate(this.args.params);
            acc = op === '+' ? acc + rhs : acc - rhs;
        }
        return acc
    },


    MulExp_iter(head: any, _ops: any, _rights: any) {
         let acc = head.calculate(this.args.params);
        const n = _rights.children.length;
        for (let i = 0; i < n; i++) {
            const op  = _ops.children[i].sourceString;
            const rhs = _rights.child(i).calculate(this.args.params);
            if (op === '*') {
                acc = acc * rhs;
            } else {
                if (rhs === 0) throw new SyntaxError('Division by zero');
                acc = acc / rhs                   
                }
        }
        return acc
    },


    UnaryExp(minuses: any, prim: any) {
        let v = prim.calculate(this.args.params);
        for (const _ of minuses.children) v = -v;
        return v;
    },

    Primary_int(n: any) {
        return parseInt(n.sourceString, 10)
    },
    Primary_ident(id: any) {
        const name = id.sourceString
        const env = this.args.params as Record <string, number>
        if (!(name in env)) return NaN;
        return env[name]
    },
    Primary_paren(_l: any, e: any, _r: any) {
        return e.calculate(this.args.params)
    },

} satisfies ArithmeticActionDict<number | undefined>;


arithSemantics.addOperation<Number>("calculate(params)", arithCalc);


export interface ArithActions {
    calculate(params: {[name:string]:number}): number;
}

export interface ArithSemantics extends ArithmeticSemantics
{
    (match: MatchResult): ArithActions;
}
