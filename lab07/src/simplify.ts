import { Expr } from "../../lab04";
import { cost } from "./cost";



export function simplify(e: Expr, identities: [Expr, Expr][]): Expr {
  
  const ext: [Expr, Expr][] = [];
  const seenPairs = new Set<string>();
  for (const [l, r] of identities) {
    for (const [a, b] of [[l, r], [r, l]] as [Expr, Expr][]) {
      const k = stringifyExpr(a) + "\u0001" + stringifyExpr(b);
      if (!seenPairs.has(k)) { seenPairs.add(k); ext.push([a, b]); }
    }
  }

  // Best-first поиск с ограничением на временное удорожание
  const START = normalize(constantFold(e));
  const startCost = cost(START);

  const COST_INCREASE_LIMIT = 5;     
  const MAX_NODES = 20_000;         

  type Node = { expr: Expr; c: number };
  // примитивная «очередь с приоритетом» на массивах 
  const pq: Node[] = [{ expr: START, c: startCost }];
  const visited = new Set<string>([keyOf(START)]);

  let best = START;
  let bestCost = startCost;
  let expanded = 0;

  while (pq.length) {
    // достаем минимальный по стоимости
    pq.sort((a, b) => a.c - b.c);
    const cur = pq.shift()!;
    const curCost = cur.c;

    if (curCost < bestCost) {
      best = cur.expr;
      bestCost = curCost;
    }

    if (++expanded > MAX_NODES) break;

    // генерим соседей (все применения тождеств на всех поддеревьях)
    const rawNext = findAllApplications(cur.expr, ext);

    // дедуп в рамках шага
    const stepSeen = new Set<string>();
    for (const n of rawNext) {
      const nf = normalize(constantFold(n));
      const nc = cost(nf);

      // ограничиваем временное удорожание
      if (nc > bestCost + COST_INCREASE_LIMIT) continue;

      const k = keyOf(nf);
      if (visited.has(k) || stepSeen.has(k)) continue;

      stepSeen.add(k);
      visited.add(k);
      pq.push({ expr: nf, c: nc });
    }
  }

  return best;
}



function constantFold(e: Expr): Expr {
  switch (e.kind) {
    case "Int":
    case "Ident":
      return e;

    case "Neg": {
      const x = constantFold(e.expr);
      if (x.kind === "Int") return { kind: "Int", value: -x.value };
      return { kind: "Neg", expr: x };
    }

    case "Add":
    case "Sub":
    case "Mul":
    case "Div": {
      const l = constantFold(e.left);
      const r = constantFold(e.right);

      if (l.kind === "Int" && r.kind === "Int") {
        const a = l.value, b = r.value;
        switch (e.kind) {
          case "Add": return { kind: "Int", value: a + b };
          case "Sub": return { kind: "Int", value: a - b };
          case "Mul": return { kind: "Int", value: a * b };
          case "Div":
            if (b === 0) return { kind: "Div", left: l, right: r };
            return { kind: "Int", value: Math.trunc(a / b) };
        }
      }
      return { kind: e.kind, left: l, right: r };
    }
  }
}



function normalize(e: Expr): Expr {
  switch (e.kind) {
    case "Int":
    case "Ident":
      return e;

    case "Neg":
      return { kind: "Neg", expr: normalize(e.expr) };

    case "Add": {
      const terms = flatten("Add", e).map(normalize);
      terms.sort((a, b) => keyOf(a).localeCompare(keyOf(b)));
      return buildAssoc("Add", terms);
    }

    case "Mul": {
      const factors = flatten("Mul", e).map(normalize);
      factors.sort((a, b) => keyOf(a).localeCompare(keyOf(b)));
      return buildAssoc("Mul", factors);
    }

    case "Sub":
      
      return { kind: "Sub", left: normalize(e.left), right: normalize(e.right) };

    case "Div":
      return { kind: "Div", left: normalize(e.left), right: normalize(e.right) };
  }
}

function flatten(kind: "Add" | "Mul", e: Expr): Expr[] {
  if (e.kind === kind) return [...flatten(kind, e.left), ...flatten(kind, e.right)];
  return [e];
}

function buildAssoc(kind: "Add" | "Mul", arr: Expr[]): Expr {
  if (arr.length === 0) return kind === "Add" ? { kind: "Int", value: 0 } : { kind: "Int", value: 1 };
  if (arr.length === 1) return arr[0];
  const mid = arr.length >> 1;
  return { kind, left: buildAssoc(kind, arr.slice(0, mid)), right: buildAssoc(kind, arr.slice(mid)) } as any;
}



function findAllApplications(e: Expr, identities: [Expr, Expr][]): Expr[] {
  const out: Expr[] = [];

  for (const [pat, repl] of identities) {
    const applied = applyIdentity(e, pat, repl);
    if (applied && !areExprsEqual(applied, e)) out.push(applied);
  }

  // рекурсивно в детях
  switch (e.kind) {
    case "Int":
    case "Ident":
      return out;

    case "Neg":
      for (const child of findAllApplications(e.expr, identities)) {
        out.push({ kind: "Neg", expr: child });
      }
      return out;

    case "Add":
    case "Sub":
    case "Mul":
    case "Div": {
      for (const l of findAllApplications(e.left, identities)) {
        out.push({ kind: e.kind, left: l, right: e.right } as any);
      }
      for (const r of findAllApplications(e.right, identities)) {
        out.push({ kind: e.kind, left: e.left, right: r } as any);
      }
      return out;
    }
  }
}

function applyIdentity(expr: Expr, pattern: Expr, replacement: Expr): Expr | null {
  const mapping = new Map<string, Expr>();
  if (matchPattern(expr, pattern, mapping)) {
    return substitute(replacement, mapping);
  }
  return null;
}

function matchPattern(expr: Expr, pattern: Expr, mapping: Map<string, Expr>): boolean {
  if (pattern.kind === "Ident") {
    const was = mapping.get(pattern.name);
    if (was) return areExprsEqual(expr, was);
    mapping.set(pattern.name, expr);
    return true;
  }
  if (expr.kind !== pattern.kind) return false;

  switch (expr.kind) {
    case "Int":
      return (pattern as any).value === expr.value;

    case "Neg":
      return matchPattern(expr.expr, (pattern as any).expr, mapping);

    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      const p: any = pattern;
      return matchPattern(expr.left, p.left, mapping) && matchPattern(expr.right, p.right, mapping);
  }
}

function substitute(expr: Expr, mapping: Map<string, Expr>): Expr {
  switch (expr.kind) {
    case "Int":
      return expr;
    case "Ident":
      return mapping.get(expr.name) || expr;
    case "Neg":
      return { kind: "Neg", expr: substitute(expr.expr, mapping) };
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return { kind: expr.kind, left: substitute(expr.left, mapping), right: substitute(expr.right, mapping) } as any;
  }
}



function areExprsEqual(a: Expr, b: Expr): boolean {
  return keyOf(a) === keyOf(b);
}

function stringifyExpr(e: Expr): string {
  switch (e.kind) {
    case "Int":   return `Int(${e.value})`;
    case "Ident": return `Ident(${e.name})`;
    case "Neg":   return `Neg(${stringifyExpr(e.expr)})`;
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return `${e.kind}(${stringifyExpr(e.left)},${stringifyExpr(e.right)})`;
  }
}

function keyOf(e: Expr): string {
  
  return stringifyExpr(normalize(e));
}
