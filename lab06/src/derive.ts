import { Expr } from "../../lab04";

const zero = (): Expr => ({ kind: "Int", value: 0 });
const one  = (): Expr => ({ kind: "Int", value: 1 });

function isZero(e: Expr): boolean { return e.kind === "Int" && e.value === 0; }
function isOne(e: Expr): boolean  { return e.kind === "Int" && e.value === 1; }
function isMinusOne(e: Expr): boolean { return e.kind === "Int" && e.value === -1; }

// проверка на равенство
function eq(a: Expr, b: Expr): boolean {
  if (a.kind !== b.kind) return false;
  switch (a.kind) {
    case "Int":   return b.kind === "Int"   && a.value === b.value;
    case "Ident": return b.kind === "Ident" && a.name  === b.name;
    case "Neg":   return eq(a.expr, (b as any).expr);
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return eq(a.left, (b as any).left) && eq(a.right, (b as any).right);
  }
}

function Neg(e: Expr): Expr {
 
  if (isZero(e)) return zero();

  // -0 = 0
  if (e.kind === "Int") {
    const v = -e.value;
    return v === 0 ? zero() : { kind: "Int", value: v };
  }

  // --x = x
  if (e.kind === "Neg") return e.expr;

  return { kind: "Neg", expr: e };
}


function Add(a: Expr, b: Expr): Expr {
  // 0 + x / x + 0
  if (isZero(a)) return b;
  if (isZero(b)) return a;

  // a + (-b) = a - b ; (-a) + b = b - a
  if (b.kind === "Neg") return Sub(a, b.expr);
  if (a.kind === "Neg") return Sub(b, a.expr);

  // константы
  if (a.kind === "Int" && b.kind === "Int") return { kind: "Int", value: a.value + b.value };

  return { kind: "Add", left: a, right: b };
}

function Sub(a: Expr, b: Expr): Expr {
  // x - 0 = x ; 0 - x = -x
  if (isZero(b)) return a;
  if (isZero(a)) return Neg(b);

  // x - x = 0
  if (eq(a, b)) return zero();

  // a - (-b) = a + b ; (-a) - b = -(a + b)
  if (b.kind === "Neg") return Add(a, b.expr);
  if (a.kind === "Neg") return Neg(Add(a.expr, b));

  // константы
  if (a.kind === "Int" && b.kind === "Int") return { kind: "Int", value: a.value - b.value };

  return { kind: "Sub", left: a, right: b };
}

function Mul(a: Expr, b: Expr): Expr {
  // x * 0 = 0 ; x * 1 = x
  if (isZero(a) || isZero(b)) return zero();
  if (isOne(a)) return b;
  if (isOne(b)) return a;

  // знаки: (-a)*(-b) = a*b ; (-a)*b = -(a*b) ; a*(-b) = -(a*b)
  if (a.kind === "Neg" && b.kind === "Neg") return Mul(a.expr, b.expr);
  if (a.kind === "Neg") return Neg(Mul(a.expr, b));
  if (b.kind === "Neg") return Neg(Mul(a, b.expr));

  // -1 * x = -x ; x * -1 = -x
  if (isMinusOne(a)) return Neg(b);
  if (isMinusOne(b)) return Neg(a);

  // константы
  if (a.kind === "Int" && b.kind === "Int") return { kind: "Int", value: a.value * b.value };

  return { kind: "Mul", left: a, right: b };
}

function Div(a: Expr, b: Expr): Expr {
  // 0/x = 0 ; x/1 = x
  if (isZero(a)) return zero();
  if (isOne(b))  return a;

  // знаки: (-a)/(-b) = a/b ; (-a)/b = -(a/b) ; a/(-b) = -(a/b)
  if (a.kind === "Neg" && b.kind === "Neg") return Div(a.expr, b.expr);
  if (a.kind === "Neg") return Neg(Div(a.expr, b));
  if (b.kind === "Neg") return Neg(Div(a, b.expr));

  // -1 / x = -(1/x) ; x / -1 = -x
  if (isMinusOne(a)) return Neg(Div(one(), b));
  if (isMinusOne(b)) return Neg(a);

  //  x/x = 1  (симв. тождество; как и всегда, вне точки x=0)
  if (eq(a, b)) return one();

  // аккуратный фолдинг для целых
  if (a.kind === "Int" && b.kind === "Int" && b.value !== 0 && a.value % b.value === 0) {
    return { kind: "Int", value: Math.trunc(a.value / b.value) };
  }
  return { kind: "Div", left: a, right: b };
}

// Доп. упрощение одного шага для уже построенных узлов
function simplify(e: Expr): Expr {
  switch (e.kind) {
    case "Int":
    case "Ident":
      return e;
    case "Neg":
      return Neg(simplify(e.expr));
    case "Add":
      return Add(simplify(e.left), simplify(e.right));
    case "Sub":
      return Sub(simplify(e.left), simplify(e.right));
    case "Mul":
      return Mul(simplify(e.left), simplify(e.right));
    case "Div":
      return Div(simplify(e.left), simplify(e.right));
  }
}

// ---------- дифференцирование ----------
export function derive(e: Expr, varName: string): Expr {
  const d = (node: Expr): Expr => {
    switch (node.kind) {
      case "Int":
        // d(c)/dx = 0
        return zero();

      case "Ident":
        // d(x)/dx = 1; d(y)/dx = 0
        return node.name === varName ? one() : zero();

      case "Neg":
        // d(-u)/dx = -(du/dx)
        return Neg(d(node.expr));

      case "Add":
        // d(u+v) = du + dv
        return Add(d(node.left), d(node.right));

      case "Sub":
        // d(u-v) = du - dv
        return Sub(d(node.left), d(node.right));

      case "Mul": {
        // d(u*v) = du*v + u*dv
        const du = d(node.left);
        const dv = d(node.right);
        return Add(Mul(du, node.right), Mul(node.left, dv));
      }

      case "Div": {
        // d(u/v) = (du*v - u*dv) / (v*v)
        const du = d(node.left);
        const dv = d(node.right);
        return Div(
          Sub(Mul(du, node.right), Mul(node.left, dv)),
          Mul(node.right, node.right)
        );
      }
    }
  };

  
  return simplify(d(e));
}