import {  MatchResult } from "ohm-js";
import { addMulSemantics } from "./calculate";
import grammar from "./addmul.ohm-bundle";

export function evaluate(content: string): number
{
    return calculate(parse(content));
}
export class SyntaxError extends Error
{
}

function parse(content: string): MatchResult
{
    throw "Not implemented";
}

function calculate(expression: MatchResult):number
{
    throw "Not implemented"
}