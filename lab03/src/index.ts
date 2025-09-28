import {  MatchResult } from "ohm-js";
import grammar  from "./arith.ohm-bundle";
import { arithSemantics } from "./calculate";

export const arithGrammar = grammar;
export {ArithmeticActionDict, ArithmeticSemantics} from './arith.ohm-bundle';

export function evaluate(content: string, params?: {[name:string]:number}): number
{
    return calculate(parse(content), params ?? {});
}
export class SyntaxError extends Error
{
    constructor(message = "Syntax error") {
    super(message);
    this.name = "SyntaxError";
  }
}

export function parse(content: string): MatchResult
{
    const match =  grammar.match(content)
    if (!match.succeeded()){
        throw new SyntaxError(match.message || "Syntax error");
    }

    return match
}

function calculate(expression: MatchResult, params: {[name:string]: number}): number
{
    return arithSemantics(expression).calculate(params);
}