import { ReversePolishNotationActionDict } from "./rpn.ohm-bundle";
import { SyntaxError } from "./index";

export const rpnStackDepth = {
      Program_seq(items) {
    let out = 0;
    let max = 0;

    for (const item of items.children) {
      
      const tokNode = item.child(0);
      const tok = tokNode.sourceString.trim();

      if (tok === "+" || tok === "*") {
        
        if (out < 2) {
          throw new SyntaxError(`stack underflow at '${tok}'`);
        }
        
        out -= 1;
      } else {
        
        const v = Number(tok);
        if (!Number.isFinite(v)) {
          throw new SyntaxError(`invalid literal: ${tok}`);
        }
        out += 1;
        if (out > max) max = out;
      }
    }

  
    if (out !== 1) {
      throw new SyntaxError(
        `expression must reduce to a single value (stack size = ${out})`
      );
    }

    return { max, out };
  },
} satisfies ReversePolishNotationActionDict<StackDepth>;
export type StackDepth = {max: number, out: number};
