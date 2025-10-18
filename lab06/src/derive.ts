import { Expr } from "../../lab04";

// ---------- маленькие конструкторы + предикаты ----------
const zero = (): Expr => ({ kind: "Int", value: 0 });
const one  = (): Expr => ({ kind: "Int", value: 1 });

function isZero(e: Expr): boolean {
  return e.kind === "Int" && e.value === 0;
}

function isOne(e: Expr): boolean {
  return e.kind === "Int" && e.value === 1;
}

// Нормализующие конструкторы с упрощениями из требований уровня B
function Neg(e: Expr): Expr {
  // --x = x
  if (e.kind === "Neg") return e.expr;
  // -0 = 0
  if (isZero(e)) return zero();

  if (e.kind === "Div") {
    // -(u/v) = (-u)/v
    return Div(Neg(e.left), e.right);
  }
  // -(u*v) = (-u)*v)
  if (e.kind === "Mul") return Mul(Neg(e.left), e.right);

  return { kind: "Neg", expr: e };
}

function Add(a: Expr, b: Expr): Expr {
  // x + 0 = x; 0 + x = x
  if (isZero(a)) return b;
  if (isZero(b)) return a;
  // Констант-фолдинг (по желанию, не обязателен)
  if (a.kind === "Int" && b.kind === "Int") return { kind: "Int", value: a.value + b.value };
  return { kind: "Add", left: a, right: b };
}

function Sub(a: Expr, b: Expr): Expr {
  // x - 0 = x
  if (isZero(b)) return a;
  // 0 - x = -x
  if (isZero(a)) return Neg(b);
  if (a.kind === "Int" && b.kind === "Int") return { kind: "Int", value: a.value - b.value };
  return { kind: "Sub", left: a, right: b };
}

function Mul(a: Expr, b: Expr): Expr {
  // x * 0 = 0 * x = 0
  if (isZero(a) || isZero(b)) return zero();
  // x * 1 = 1 * x = x
  if (isOne(a)) return b;
  if (isOne(b)) return a;
  if (a.kind === "Int" && b.kind === "Int") return { kind: "Int", value: a.value * b.value };
  return { kind: "Mul", left: a, right: b };
}

function Div(a: Expr, b: Expr): Expr {
  // x / 1 = x
  if (isOne(b)) return a;
  // 0 / x = 0 (кроме x = 0, но символически оставим 0)
  if (isZero(a)) return zero();
  if (a.kind === "Int" && b.kind === "Int") {
    // аккуратный фолдинг только если делится нацело
    if (b.value !== 0 && a.value % b.value === 0) {
      return { kind: "Int", value: Math.trunc(a.value / b.value) };
    }
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

  // Один прогон упрощений поверх результата
  return simplify(d(e));
}