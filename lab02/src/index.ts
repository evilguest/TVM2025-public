import grammar from "./rpn.ohm-bundle";
import { rpnSemantics } from "./semantics";
import { MatchResult } from "ohm-js";
import { StackDepth } from "./stackDepth";

export function evaluate(source: string): number
{
    return calculate(parse(source));
}

export function maxStackDepth(source: string): number
{ 
    const m = parse(source)
    const sd = rpnSemantics(m).stackDepth
    return sd.max
}

function parse(content: string): MatchResult
{
    const match =  grammar.match(content)
    if (!match.succeeded()){
        throw new SyntaxError(match.message || "Syntax error");
    }

    return match
}

function calculate(expression: MatchResult): number {
  return rpnSemantics(expression).calculate();
}


export class SyntaxError extends Error
{
    constructor(message = "Syntax error") {
    super(message);
    this.name = "SyntaxError";
  }
}

