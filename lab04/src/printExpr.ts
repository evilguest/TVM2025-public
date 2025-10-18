import { Expr } from "./ast";

const enum Prec { AddSub = 1, MulDiv = 2, Neg = 3, Atom = 4 }

function myPrec(e: Expr): number {
  switch (e.kind) {
    case "Int":
    case "Ident": return Prec.Atom;
    case "Neg": return Prec.Neg;
    case "Mul":
    case "Div": return Prec.MulDiv;
    case "Add":
    case "Sub": return Prec.AddSub;
  }
}

const OP: Record<"Add"|"Sub"|"Mul"|"Div", string> = {
  Add: "+", Sub: "-", Mul: "*", Div: "/"
};

// side: 0 — левый, 1 — правый
function emit(
  e: Expr,
  parentPrec: number,
  parentKind: Expr["kind"] | null,
  side: 0 | 1
): string {
  const p = myPrec(e); // приоритет текущего узла

  switch (e.kind) {
    case "Int": {
      const s = String(e.value);
      // атом никогда не нуждается во внешних скобках по нашей логике
      return s;
    }

    case "Ident": {
      const s = e.name;
      return s;
    }

    case "Neg": {
      // скобки нужны только если внутренний узел слабее унарного
      const inner = emit(e.expr, Prec.Neg, "Neg", 1);
      const s = `-${inner}`;
      // ставить скобки вокруг всего унарного узла почти никогда не надо,
      // решает общий критерий ниже
      const needOuter =
        p < parentPrec ||
        (side === 1 && p === parentPrec && (parentKind === "Sub" || parentKind === "Div"));
      return needOuter ? `(${s})` : s;
    }

    case "Add":
    case "Sub":
    case "Mul":
    case "Div": {
      const sL = emit(e.left,  p, e.kind, 0);
      const sR = emit(e.right, p, e.kind, 1);
      const s  = `${sL} ${OP[e.kind]} ${sR}`;

      // общий критерий скобок для бинарных:
      // 1) текущий слабее родителя
      // 2) равный приоритет и мы правый сын у Sub/Div
      const needOuter =
        p < parentPrec ||
        (side === 1 && p === parentPrec && (parentKind === "Sub" || parentKind === "Div"));

      return needOuter ? `(${s})` : s;
    }
  }
}

export function printExpr(e: Expr): string {
  return emit(e, 0, null, 0);
}
