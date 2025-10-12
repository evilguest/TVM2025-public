import { Dict, MatchResult, Semantics } from "ohm-js";
import grammar from "./rpn.ohm-bundle";
import { rpnStackDepth, StackDepth } from "./stackDepth";
import { rpnCalc } from "./calculate";

interface RpnDict extends Dict {
    calculate(): number;
    stackDepth: StackDepth;
}

interface RpnSemantics extends Semantics
{
    (match: MatchResult): RpnDict;
}

export const rpnSemantics: RpnSemantics = grammar.createSemantics() as RpnSemantics;
rpnSemantics.addOperation<number>("calculate()", rpnCalc);
rpnSemantics.addAttribute<StackDepth>("stackDepth", rpnStackDepth);