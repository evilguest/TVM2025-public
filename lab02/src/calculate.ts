import { ReversePolishNotationActionDict} from "./rpn.ohm-bundle";
import { SyntaxError } from "./index";

type Node = { calculate(): number };


export const rpnCalc = {
   Program_seq(items) {
       const st: Node[] = [];
    for (const it of items.children) {
      const t = it.child(0).sourceString.trim();
      if (t === '+') {
        if (st.length < 2) throw new SyntaxError("stack underflow at '+'");
        const b = st.pop()!, a = st.pop()!;
        st.push({ calculate: () => a.calculate() + b.calculate() });
      } else if (t === '*') {
        if (st.length < 2) throw new SyntaxError("stack underflow at '*'");
        const b = st.pop()!, a = st.pop()!;
        st.push({ calculate: () => a.calculate() * b.calculate() });
      } else {
        const v = Number(t);
        if (!Number.isFinite(v)) throw new SyntaxError(`invalid number: ${t}`);
        st.push({ calculate: () => v });
      }
    }
    if (st.length !== 1) {
      throw new SyntaxError(`expression must reduce to a single value (stack size = ${st.length})`);
    }
    return st[0].calculate();
  },
   
} satisfies ReversePolishNotationActionDict<number>;

