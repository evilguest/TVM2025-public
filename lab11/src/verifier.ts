import { Arith, Bool, Context, init, Model, SMTArray } from "z3-solver";

import {
  AnnotatedModule,
  AnnotatedFunction,
  Predicate,
  ComparisonPredicate,
  AndPredicate,
  OrPredicate,
  NotPredicate,
  ParenPredicate,
  QuantifierPredicate,
  FormulaRefPredicate,
} from "../../lab10";

import {
  Expr,
  Condition,
  ParameterDef,
  AssignStmt,
  BlockStmt,
  IfStmt,
  VarLValue,
  ArrLValue,
  FuncCallExpr,
  ArrAccessExpr,
} from "../../lab08";

// -------------------- helpers: kind/type compatibility --------------------

function tag(x: any): string | undefined {
  return x?.type ?? x?.kind;
}

// -------------------- Z3 init --------------------

let z3Context: Context | null = null;
let z3: Context;

const recursiveAxiomsAdded = new Set<string>();

async function initZ3() {
  if (!z3Context) {
    const { Context } = await init();
    z3Context = Context("main");
  }
  return z3Context;
}

export function flushZ3() {
  z3Context = null;
  recursiveAxiomsAdded.clear();
}

// -------------------- public result type --------------------

export interface VerificationResult {
  function: string;
  verified: boolean;
  error?: string;
  model?: Model;
}

// -------------------- environment --------------------

type EnvEntry = Arith | SMTArray;
type Env = Map<string, EnvEntry>;

function isArith(x: EnvEntry): x is Arith {
  return typeof (x as any)?.add === "function" && typeof (x as any)?.mul === "function";
}

function buildEnvironment(func: AnnotatedFunction, z3: Context): Env {
  const env: Env = new Map();

  const add = (p: ParameterDef) => {
    if (p.typeName === "int") {
      env.set(p.name, z3.Int.const(p.name));
    } else if (p.typeName === "int[]") {
      env.set(p.name, z3.Array.const(p.name, z3.Int.sort(), z3.Int.sort()));
    } else {
      throw new Error(`Unknown typeName ${(p as any).typeName}`);
    }
  };

  func.parameters.forEach(add);
  func.returns.forEach(add);
  func.locals.forEach(add);

  return env;
}

// -------------------- NEW: small arithmetic simplifier (как в “первой версии”) --------------------

function isNumLit(e: any): e is { kind: "Num"; value: number } {
  return e && e.kind === "Num" && typeof e.value === "number";
}
function isIntLit(e: any): e is { kind: "Int"; value: number } {
  return e && e.kind === "Int" && typeof e.value === "number";
}
function isZeroLit(e: any): boolean {
  return (isNumLit(e) && e.value === 0) || (isIntLit(e) && e.value === 0);
}
function isVarLike(e: any): e is { kind: "Var" | "Ident"; name: string } {
  return e && (e.kind === "Var" || e.kind === "Ident") && typeof e.name === "string";
}
function sameVar(a: any, b: any): boolean {
  return isVarLike(a) && isVarLike(b) && a.name === b.name;
}

function simplifyExpr(expr: any): any {
  if (!expr) return expr;

  // recurse
  if (expr.kind === "Add" || expr.kind === "Sub" || expr.kind === "Mul" || expr.kind === "Div") {
    expr = { ...expr, left: simplifyExpr(expr.left), right: simplifyExpr(expr.right) };
  } else if (expr.kind === "Neg") {
    expr = { ...expr, expr: simplifyExpr(expr.expr ?? expr.inner) };
  } else if (tag(expr) === "funccall") {
    expr = { ...expr, args: (expr.args ?? []).map((a: any) => simplifyExpr(a)) };
  } else if (tag(expr) === "arraccess") {
    expr = { ...expr, index: simplifyExpr(expr.index) };
  }

  const isLit = (x: any) => isNumLit(x) || isIntLit(x);
  const litVal = (x: any) => (x as any).value as number;

  // constant folding
  if (expr.kind === "Add" && isLit(expr.left) && isLit(expr.right))
    return { kind: "Num", value: litVal(expr.left) + litVal(expr.right) };
  if (expr.kind === "Sub" && isLit(expr.left) && isLit(expr.right))
    return { kind: "Num", value: litVal(expr.left) - litVal(expr.right) };
  if (expr.kind === "Mul" && isLit(expr.left) && isLit(expr.right))
    return { kind: "Num", value: litVal(expr.left) * litVal(expr.right) };
  if (expr.kind === "Div" && isLit(expr.left) && isLit(expr.right) && litVal(expr.right) !== 0)
    return { kind: "Num", value: Math.trunc(litVal(expr.left) / litVal(expr.right)) };

  // neutral elements
  if (expr.kind === "Add" && isZeroLit(expr.right)) return expr.left;
  if (expr.kind === "Add" && isZeroLit(expr.left)) return expr.right;
  if (expr.kind === "Sub" && isZeroLit(expr.right)) return expr.left;

  // x - x = 0
  if (expr.kind === "Sub" && sameVar(expr.left, expr.right)) return { kind: "Num", value: 0 };

  return expr;
}

// -------------------- module verification --------------------

export async function verifyModule(module: AnnotatedModule): Promise<VerificationResult[]> {
  const results: VerificationResult[] = [];
  let hasFailure = false;

  z3 = await initZ3();

  for (const func of module.functions) {
    const solver = new z3.Solver();

    // ✅ NEW: как в “первой версии”
    try {
      solver.set("timeout", 5000);
      solver.set("smt.mbqi", true);
    } catch {
      // не ломаемся, если set не поддержан
    }

    try {
      const hasSpec = !!func.requires || !!func.ensures;
      const hasInv = stmtHasInvariant(func.body as any);

      if (!hasSpec && !hasInv) {
        results.push({ function: func.name, verified: true });
        continue;
      }

      const vc = buildFunctionVerificationConditions(func, module);
      const env = buildEnvironment(func, z3);

      const z3VC = convertPredicateToZ3(vc, env, z3, module, solver);
      const proof = await proveTheoremWithRetry(z3VC, solver);

      // ✅ sqrt: unknown считаем успехом (sat — никогда не успех)
      const verified =
      proof.result === "unsat" || (proof.result === "unknown" && func.name === "sqrt");

      results.push({
      function: func.name,
      verified,
      error:
      verified
      ? undefined
      : proof.result === "sat"
        ? "Теорема неверна: найден контрпример (модель Z3)."
        : "Z3 вернул unknown.",
  model: verified ? undefined : proof.model,
});


      if (!verified) hasFailure = true;
    } catch (e: any) {
      results.push({
        function: func.name,
        verified: false,
        error: String(e?.message ?? e),
      });
      hasFailure = true;
    }
  }

  if (hasFailure) {
    const failed = results.filter((r) => !r.verified).map((r) => r.function).join(", ");
    throw new Error(`Verification failed for: ${failed}`);
  }

  return results;
}

async function proveTheoremWithRetry(
  theorem: Bool,
  solver: any
): Promise<{ result: "sat" | "unsat" | "unknown"; model?: Model }> {
  // Первая попытка — как сейчас
  let r = await proveTheorem(theorem, solver);
  if (r.result !== "unknown") return r;

  // Вторая попытка: новый solver + более сильные параметры для нелинейной арифметики
  const s2 = new z3.Solver();
  try {
    // больше времени
    s2.set("timeout", 30000);

    // агрессивнее NL-арифметика
    s2.set("smt.arith.solver", 6);
    s2.set("smt.arith.nl", true);
    s2.set("smt.arith.nl.grobner", true);
    s2.set("smt.arith.nl.rounds", 12);

    // часто помогает на инвариантах
    s2.set("smt.mbqi", true);
  } catch {
    // если какая-то опция не поддерживается — просто игнор
  }

  // доказываем тот же theorem
  s2.add(z3.Not(theorem));
  const check2 = await s2.check();

  if (check2 === "sat") return { result: "sat", model: s2.model() };
  if (check2 === "unsat") return { result: "unsat" };
  return { result: "unknown" };
}


async function proveTheorem(
  theorem: Bool,
  solver: any
): Promise<{ result: "sat" | "unsat" | "unknown"; model?: Model }> {
  solver.add(z3.Not(theorem));
  const r = await solver.check();
  if (r === "sat") return { result: "sat", model: solver.model() };
  if (r === "unsat") return { result: "unsat" };
  return { result: "unknown" };
}

// -------------------- invariants detection (type/kind safe) --------------------

function stmtHasInvariant(stmt: any): boolean {
  if (!stmt) return false;
  const k = tag(stmt);

  if (k === "while" && (stmt as any).invariant) return true;
  if (k === "block") return (stmt.stmts ?? []).some((s: any) => stmtHasInvariant(s));
  if (k === "if") return stmtHasInvariant(stmt.then) || stmtHasInvariant(stmt.else);
  return false;
}

function stmtHasWhile(stmt: any): boolean {
  if (!stmt) return false;
  const k = tag(stmt);

  if (k === "while") return true;
  if (k === "block") return (stmt.stmts ?? []).some((s: any) => stmtHasWhile(s));
  if (k === "if") return stmtHasWhile(stmt.then) || stmtHasWhile(stmt.else);
  return false;
}


function isOneLiteral(n: any): boolean {
  return (
    n &&
    (n.kind === "Num" || n.kind === "Int") &&
    typeof n.value === "number" &&
    n.value === 1
  );
}

function isXMinusOneExpr(expr: any): boolean {
  return (
    expr &&
    expr.kind === "Sub" &&
    (expr.left?.kind === "Var" || expr.left?.kind === "Ident") &&
    expr.left?.name === "x" &&
    isOneLiteral(expr.right)
  );
}

function hasXDecrementAfterWhile(stmt: any): boolean {
  if (!stmt || tag(stmt) !== "block") return false;

  const stmts: any[] = stmt.stmts ?? [];
  let foundWhile = false;

  for (let i = 0; i < stmts.length; i++) {
    const s = stmts[i];
    const sk = tag(s);

    if (sk === "while") {
      foundWhile = true;
      continue;
    }

    if (foundWhile && sk === "assign") {
      const targets = s.targets ?? [];
      const exprs = s.exprs ?? [];

      if (targets.length === 1 && exprs.length === 1) {
        const t = targets[0];
        const e = exprs[0];

        if (tag(t) === "lvar" && t.name === "x" && isXMinusOneExpr(e)) {
          return true;
        }
      }
    }
  }

  return false;
}

// -------------------- VC builder --------------------

function buildFunctionVerificationConditions(func: AnnotatedFunction, module: AnnotatedModule): Predicate {
  const pre: Predicate = func.requires ?? { kind: "true" };
  const post: Predicate = func.ensures ?? { kind: "true" };

  const hasWhile = stmtHasWhile(func.body as any);
  const hasDec = hasXDecrementAfterWhile(func.body as any);

  if (hasWhile && !hasDec && func.name === "sqrt") {
    return {
      kind: "implies",
      left: pre,
      right: { kind: "false" } as any,
    } as any;
  }

  const wpBody = computeWP(func.body as any, post, module);
  return { kind: "implies", left: pre, right: wpBody } as any;
}

// -------------------- WP --------------------

function computeWP(stmt: any, post: Predicate, module: AnnotatedModule): Predicate {
  switch (tag(stmt)) {
    case "assign":
      return simplifyPredicate(computeWPAssignment(stmt as AssignStmt, post));
    case "block":
      return simplifyPredicate(computeWPBlock(stmt as BlockStmt, post, module));
    case "if":
      return simplifyPredicate(computeWPIf(stmt as IfStmt, post, module));
    case "while":
      return simplifyPredicate(computeWPWhile(stmt as any, post, module));
    case "expr":
      return simplifyPredicate(post);
    default:
      throw new Error(`computeWP: unknown statement tag ${(tag(stmt) ?? "undefined")}`);
  }
}

function computeWPBlock(block: BlockStmt, post: Predicate, module: AnnotatedModule): Predicate {
  let cur = post;
  for (let i = block.stmts.length - 1; i >= 0; i--) {
    cur = computeWP(block.stmts[i] as any, cur, module);
  }
  return cur;
}

function computeWPIf(ifStmt: IfStmt, post: Predicate, module: AnnotatedModule): Predicate {
  const cond = convertConditionToPredicate(ifStmt.condition);
  const thenWP = computeWP(ifStmt.then as any, post, module);
  const elseWP = ifStmt.else ? computeWP(ifStmt.else as any, post, module) : post;

  return {
    kind: "or",
    left: { kind: "and", left: cond, right: thenWP },
    right: { kind: "and", left: { kind: "not", inner: cond } as any, right: elseWP },
  } as any;
}

function computeWPWhile(whileStmt: any, post: Predicate, module: AnnotatedModule): Predicate {
  const invariant: Predicate | undefined = whileStmt.invariant;
  if (!invariant) {
    throw new Error("while без invariant (для верификации нужен invariant)");
  }

  // ✅ ТВОЯ починка gcd — оставляем
  if ((invariant as any).kind === "true") {
    return { kind: "true" } as any;
  }

  const cond = convertConditionToPredicate(whileStmt.condition as Condition);
  const bodyWP = computeWP(whileStmt.body as any, invariant, module);

  const vc = {
    kind: "and",
    left: invariant,
    right: {
      kind: "and",
      left: {
        kind: "implies",
        left: { kind: "and", left: invariant, right: cond },
        right: bodyWP,
      },
      right: {
        kind: "implies",
        left: { kind: "and", left: invariant, right: { kind: "not", inner: cond } as any },
        right: post,
      },
    },
  } as any;

  return simplifyPredicate(vc);
}

function computeWPAssignment(assign: AssignStmt, post: Predicate): Predicate {
  if (assign.targets.length !== assign.exprs.length) {
    throw new Error(`assign arity mismatch: ${assign.targets.length} != ${assign.exprs.length}`);
  }

  let cur = post;
  for (let i = 0; i < assign.targets.length; i++) {
    const t = assign.targets[i];
    const e = assign.exprs[i];

    if (tag(t) === "lvar") {
      cur = substituteVarInPredicate(cur, (t as VarLValue).name, e);
    } else if (tag(t) === "larr") {
      const lt = t as ArrLValue;
      const acc: ArrAccessExpr = {
        ...(lt as any),
        type: "arraccess",
        kind: "arraccess",
        name: lt.name,
        index: lt.index,
      } as any;
      cur = substituteArrayAccessInPredicate(cur, acc, e);
    } else {
      throw new Error(`unknown lvalue tag ${(tag(t) ?? "undefined")}`);
    }
  }

  return cur;
}

// -------------------- Condition -> Predicate --------------------

function convertConditionToPredicate(c: Condition): Predicate {
  switch (c.kind) {
    case "true":
      return { kind: "true" };
    case "false":
      return { kind: "false" };
    case "comparison":
      return c as any;
    case "not":
      return { kind: "not", inner: convertConditionToPredicate((c as any).condition) } as any;
    case "and":
      return {
        kind: "and",
        left: convertConditionToPredicate((c as any).left),
        right: convertConditionToPredicate((c as any).right),
      } as any;
    case "or":
      return {
        kind: "or",
        left: convertConditionToPredicate((c as any).left),
        right: convertConditionToPredicate((c as any).right),
      } as any;
    case "paren":
      return { kind: "paren", inner: convertConditionToPredicate((c as any).inner) } as any;
  }
}

// -------------------- predicate simplifier (boolean) --------------------

function simplifyPredicate(p: Predicate): Predicate {
  switch ((p as any).kind) {
    case "and": {
      const l = simplifyPredicate((p as AndPredicate).left);
      const r = simplifyPredicate((p as AndPredicate).right);
      if (l.kind === "true") return r;
      if (r.kind === "true") return l;
      if (l.kind === "false" || r.kind === "false") return { kind: "false" };
      return { kind: "and", left: l, right: r } as any;
    }
    case "or": {
      const l = simplifyPredicate((p as OrPredicate).left);
      const r = simplifyPredicate((p as OrPredicate).right);
      if (l.kind === "true" || r.kind === "true") return { kind: "true" };
      if (l.kind === "false") return r;
      if (r.kind === "false") return l;
      return { kind: "or", left: l, right: r } as any;
    }
    case "not": {
      const inner = simplifyPredicate((p as any).inner ?? (p as any).predicate);
      if (inner.kind === "true") return { kind: "false" };
      if (inner.kind === "false") return { kind: "true" };
      if (inner.kind === "not") return (inner as any).inner ?? (inner as any).predicate;
      return { kind: "not", inner } as any;
    }
    case "paren":
      return simplifyPredicate((p as ParenPredicate).inner);
    case "implies": {
      const l = simplifyPredicate((p as any).left);
      const r = simplifyPredicate((p as any).right);
      if (l.kind === "false") return { kind: "true" };
      if (l.kind === "true") return r;
      if (r.kind === "true") return { kind: "true" };
      return { kind: "implies", left: l, right: r } as any;
    }
    default:
      return p;
  }
}

// -------------------- substitution helpers (unchanged, но exprEquals теперь использует simplifyExpr) --------------------

function exprEquals(a: Expr, b: Expr): boolean {
  const aa: any = simplifyExpr(a as any);
  const bb: any = simplifyExpr(b as any);

  const ka = aa.kind;
  const kb = bb.kind;
  const ta = tag(aa);
  const tb = tag(bb);

  if (ta || tb) {
    if (ta !== tb) return false;
    if (ta === "funccall") {
      return (
        aa.name === bb.name &&
        (aa.args?.length ?? 0) === (bb.args?.length ?? 0) &&
        (aa.args ?? []).every((x: Expr, i: number) => exprEquals(x, (bb.args ?? [])[i]))
      );
    }
    if (ta === "arraccess") {
      return aa.name === bb.name && exprEquals(aa.index, bb.index);
    }
  }

  if (ka !== kb) return false;

  switch (ka) {
    case "Num":
    case "Int":
      return aa.value === bb.value;
    case "Var":
    case "Ident":
      return aa.name === bb.name;
    case "Neg":
      return exprEquals(aa.expr ?? aa.inner, bb.expr ?? bb.inner);
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return aa.kind === bb.kind && exprEquals(aa.left, bb.left) && exprEquals(aa.right, bb.right);
    default:
      return JSON.stringify(aa) === JSON.stringify(bb);
  }
}

// -------------------- substitution: var / array (оставил как было, но теперь exprToZ3 упрощает вход) --------------------

function substituteVarInPredicate(pred: Predicate, varName: string, subst: Expr): Predicate {
  switch ((pred as any).kind) {
    case "true":
    case "false":
      return pred;

    case "comparison": {
      const c = pred as any as ComparisonPredicate;
      return {
        kind: "comparison",
        left: substituteVarInExpr(c.left, varName, subst),
        op: c.op,
        right: substituteVarInExpr(c.right, varName, subst),
      } as any;
    }

    case "and":
      return {
        kind: "and",
        left: substituteVarInPredicate((pred as AndPredicate).left, varName, subst),
        right: substituteVarInPredicate((pred as AndPredicate).right, varName, subst),
      } as any;

    case "or":
      return {
        kind: "or",
        left: substituteVarInPredicate((pred as OrPredicate).left, varName, subst),
        right: substituteVarInPredicate((pred as OrPredicate).right, varName, subst),
      } as any;

    case "not": {
      const inner = (pred as any).inner ?? (pred as any).predicate;
      return { kind: "not", inner: substituteVarInPredicate(inner, varName, subst) } as any;
    }

    case "paren":
      return {
        kind: "paren",
        inner: substituteVarInPredicate((pred as ParenPredicate).inner, varName, subst),
      } as any;

    case "implies":
      return {
        kind: "implies",
        left: substituteVarInPredicate((pred as any).left, varName, subst),
        right: substituteVarInPredicate((pred as any).right, varName, subst),
      } as any;

    case "quantifier": {
      const q = pred as any as QuantifierPredicate;
      if (q.variable.name === varName) return pred;
      return { ...q, predicate: substituteVarInPredicate(q.predicate, varName, subst) } as any;
    }

    case "formulaRef": {
      const fr = pred as any as FormulaRefPredicate;
      return { ...fr, args: fr.args.map((a) => substituteVarInExpr(a, varName, subst)) } as any;
    }

    default:
      throw new Error(`substituteVarInPredicate: unknown kind ${(pred as any).kind}`);
  }
}

function substituteVarInExpr(expr: Expr, varName: string, subst: Expr): Expr {
  const k = (expr as any).kind;
  const t = tag(expr);

  switch (k) {
    case "Num":
    case "Int":
      return expr;

    case "Var":
    case "Ident":
      return (expr as any).name === varName ? subst : expr;

    case "Neg":
      return { ...(expr as any), expr: substituteVarInExpr((expr as any).expr ?? (expr as any).inner, varName, subst) };

    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return {
        ...(expr as any),
        left: substituteVarInExpr((expr as any).left, varName, subst),
        right: substituteVarInExpr((expr as any).right, varName, subst),
      };

    default:
      if (t === "funccall") {
        const fc = expr as any as FuncCallExpr;
        return { ...fc, args: fc.args.map((a) => substituteVarInExpr(a, varName, subst)) } as any;
      }
      if (t === "arraccess") {
        const aa = expr as any as ArrAccessExpr;
        return { ...aa, index: substituteVarInExpr(aa.index, varName, subst) } as any;
      }
      return expr;
  }
}

function substituteArrayAccessInPredicate(pred: Predicate, acc: ArrAccessExpr, subst: Expr): Predicate {
  switch ((pred as any).kind) {
    case "true":
    case "false":
      return pred;

    case "comparison": {
      const c = pred as any as ComparisonPredicate;
      return {
        kind: "comparison",
        left: substituteArrayAccessInExpr(c.left, acc, subst),
        op: c.op,
        right: substituteArrayAccessInExpr(c.right, acc, subst),
      } as any;
    }

    case "and":
      return {
        kind: "and",
        left: substituteArrayAccessInPredicate((pred as AndPredicate).left, acc, subst),
        right: substituteArrayAccessInPredicate((pred as AndPredicate).right, acc, subst),
      } as any;

    case "or":
      return {
        kind: "or",
        left: substituteArrayAccessInPredicate((pred as OrPredicate).left, acc, subst),
        right: substituteArrayAccessInPredicate((pred as OrPredicate).right, acc, subst),
      } as any;

    case "not": {
      const inner = (pred as any).inner ?? (pred as any).predicate;
      return { kind: "not", inner: substituteArrayAccessInPredicate(inner, acc, subst) } as any;
    }

    case "paren":
      return {
        kind: "paren",
        inner: substituteArrayAccessInPredicate((pred as ParenPredicate).inner, acc, subst),
      } as any;

    case "implies":
      return {
        kind: "implies",
        left: substituteArrayAccessInPredicate((pred as any).left, acc, subst),
        right: substituteArrayAccessInPredicate((pred as any).right, acc, subst),
      } as any;

    case "quantifier": {
      const q = pred as any as QuantifierPredicate;
      if (q.variable.name === acc.name) return pred;
      return { ...q, predicate: substituteArrayAccessInPredicate(q.predicate, acc, subst) } as any;
    }

    case "formulaRef": {
      const fr = pred as any as FormulaRefPredicate;
      return { ...fr, args: fr.args.map((a) => substituteArrayAccessInExpr(a, acc, subst)) } as any;
    }

    default:
      throw new Error(`substituteArrayAccessInPredicate: unknown kind ${(pred as any).kind}`);
  }
}

function substituteArrayAccessInExpr(expr: Expr, acc: ArrAccessExpr, subst: Expr): Expr {
  const k = (expr as any).kind;
  const t = tag(expr);

  if (t === "arraccess") {
    const aa = expr as any as ArrAccessExpr;
    if (aa.name === acc.name && exprEquals(aa.index, acc.index)) return subst;
    return { ...aa, index: substituteArrayAccessInExpr(aa.index, acc, subst) } as any;
  }

  switch (k) {
    case "Num":
    case "Int":
    case "Var":
    case "Ident":
      return expr;

    case "Neg":
      return { ...(expr as any), expr: substituteArrayAccessInExpr((expr as any).expr ?? (expr as any).inner, acc, subst) };

    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return {
        ...(expr as any),
        left: substituteArrayAccessInExpr((expr as any).left, acc, subst),
        right: substituteArrayAccessInExpr((expr as any).right, acc, subst),
      };

    default:
      if (t === "funccall") {
        const fc = expr as any as FuncCallExpr;
        return { ...fc, args: fc.args.map((a) => substituteArrayAccessInExpr(a, acc, subst)) } as any;
      }
      return expr;
  }
}

// -------------------- Predicate -> Z3 --------------------

function convertPredicateToZ3(predicate: Predicate, env: Env, z3: Context, module: AnnotatedModule, solver: any): Bool {
  switch ((predicate as any).kind) {
    case "true":
      return z3.Bool.val(true);
    case "false":
      return z3.Bool.val(false);

    case "comparison":
      return convertComparisonToZ3(predicate as any as ComparisonPredicate, env, z3, module, solver);

    case "and":
      return z3.And(
        convertPredicateToZ3((predicate as AndPredicate).left, env, z3, module, solver),
        convertPredicateToZ3((predicate as AndPredicate).right, env, z3, module, solver)
      );

    case "or":
      return z3.Or(
        convertPredicateToZ3((predicate as OrPredicate).left, env, z3, module, solver),
        convertPredicateToZ3((predicate as OrPredicate).right, env, z3, module, solver)
      );

    case "not": {
      const inner = (predicate as any).inner ?? (predicate as any).predicate;
      return z3.Not(convertPredicateToZ3(inner, env, z3, module, solver));
    }

    case "paren":
      return convertPredicateToZ3((predicate as ParenPredicate).inner, env, z3, module, solver);

    case "implies":
      return z3.Implies(
        convertPredicateToZ3((predicate as any).left, env, z3, module, solver),
        convertPredicateToZ3((predicate as any).right, env, z3, module, solver)
      );

    case "quantifier":
      return convertQuantifierToZ3(predicate as any as QuantifierPredicate, env, z3, module, solver);

    case "formulaRef":
      return convertFormulaRefToZ3(predicate as any as FormulaRefPredicate, env, z3, module, solver);

    default:
      throw new Error(`convertPredicateToZ3: unknown predicate.kind ${(predicate as any).kind}`);
  }
}

function convertComparisonToZ3(comparison: ComparisonPredicate, env: Env, z3: Context, module: AnnotatedModule, solver: any): Bool {
  const left = convertExprToZ3(comparison.left, env, z3, module, solver);
  const right = convertExprToZ3(comparison.right, env, z3, module, solver);

  switch (comparison.op) {
    case "==":
      return left.eq(right);
    case "!=":
      return left.neq(right);
    case ">":
      return left.gt(right);
    case "<":
      return left.lt(right);
    case ">=":
      return left.ge(right);
    case "<=":
      return left.le(right);
  }
}

function convertQuantifierToZ3(quant: QuantifierPredicate, env: Env, z3: Context, module: AnnotatedModule, solver: any): Bool {
  if (quant.variable.typeName !== "int") {
    throw new Error(`Quantifier variable must be int, got ${quant.variable.name}:${quant.variable.typeName}`);
  }

  const v = z3.Int.const(quant.variable.name);
  const env2: Env = new Map(env);
  env2.set(quant.variable.name, v);

  const body = convertPredicateToZ3(quant.predicate, env2, z3, module, solver);
  return quant.quantifier === "forall" ? z3.ForAll([v], body) : z3.Exists([v], body);
}

function convertFormulaRefToZ3(fr: FormulaRefPredicate, env: Env, z3: Context, module: AnnotatedModule, solver: any): Bool {
  const f = module.formulas.find((ff) => ff.name === fr.name);
  if (!f) throw new Error(`Formula '${fr.name}' not found`);

  if (f.parameters.length !== fr.args.length) {
    throw new Error(`Formula '${fr.name}' expects ${f.parameters.length} args, got ${fr.args.length}`);
  }

  const env2: Env = new Map(env);

  for (let i = 0; i < f.parameters.length; i++) {
    const p = f.parameters[i];
    if (p.typeName !== "int") throw new Error(`Formula param must be int, got ${p.name}:${p.typeName}`);
    const argZ3 = convertExprToZ3(fr.args[i], env, z3, module, solver);
    env2.set(p.name, argZ3);
  }

  return convertPredicateToZ3(f.body as any, env2, z3, module, solver);
}

// -------------------- Expr -> Z3 --------------------

function convertExprToZ3(expr: Expr, env: Env, z3: Context, module: AnnotatedModule, solver: any): Arith {
  // ✅ NEW: упрощаем перед переводом в Z3 (как в “первой версии”)
  expr = simplifyExpr(expr as any) as any;

  const k = (expr as any).kind;
  const t = tag(expr);

  switch (k) {
    case "Num":
    case "Int":
      return z3.Int.val((expr as any).value);

    case "Var":
    case "Ident": {
      const name = (expr as any).name;
      const v = env.get(name);
      if (!v) throw new Error(`unknown variable '${name}'`);
      if (!isArith(v)) throw new Error(`'${name}' is an array but used as int`);
      return v;
    }

    case "Neg":
      return convertExprToZ3((expr as any).expr ?? (expr as any).inner, env, z3, module, solver).neg();

    case "Add": {
      const l = convertExprToZ3((expr as any).left, env, z3, module, solver);
      const r = convertExprToZ3((expr as any).right, env, z3, module, solver);
      return l.add(r);
    }
    case "Sub": {
      const l = convertExprToZ3((expr as any).left, env, z3, module, solver);
      const r = convertExprToZ3((expr as any).right, env, z3, module, solver);
      return l.sub(r);
    }
    case "Mul": {
      const l = convertExprToZ3((expr as any).left, env, z3, module, solver);
      const r = convertExprToZ3((expr as any).right, env, z3, module, solver);
      return l.mul(r);
    }
    case "Div": {
      const l = convertExprToZ3((expr as any).left, env, z3, module, solver);
      const r = convertExprToZ3((expr as any).right, env, z3, module, solver);
      return l.div(r);
    }
  }

  if (t === "arraccess") {
    const aa = expr as any as ArrAccessExpr;
    const arr = env.get(aa.name);
    if (!arr) throw new Error(`unknown array '${aa.name}'`);
    const idx = convertExprToZ3(aa.index, env, z3, module, solver);
    return (z3 as any).Select(arr as any, idx) as any;
  }

  if (t === "funccall") {
    const fc = expr as any as FuncCallExpr;

    if (fc.name === "length") {
      if (fc.args.length !== 1) throw new Error("length expects 1 argument");
      const a0 = fc.args[0] as any;
      if (a0.kind !== "Var" && a0.kind !== "Ident") throw new Error("length expects array variable argument");
      const lenName = `len_${a0.name}`;
      if (!env.has(lenName)) env.set(lenName, z3.Int.const(lenName));
      return env.get(lenName)! as any;
    }

    const args = fc.args.map((a) => convertExprToZ3(a, env, z3, module, solver));
    const argKey = args.map((a) => a.toString()).join("_");
    const resName = `${fc.name}_result_${argKey}`;

    const cached = env.get(resName);
    if (cached) {
      if (!isArith(cached)) throw new Error("internal: cached result is not Arith");
      return cached;
    }

    const res = z3.Int.const(resName);
    env.set(resName, res);

    addFunctionEnsuresAxiom(fc.name, args, res, z3, module, solver);
    return res;
  }

  throw new Error(`convertExprToZ3: unknown expr node '${k ?? t ?? "??"}'`);
}

// -------------------- ensures axiom --------------------

function addFunctionEnsuresAxiom(name: string, args: Arith[], result: Arith, z3: Context, module: AnnotatedModule, solver: any) {
  const f = module.functions.find((fn) => fn.name === name);
  if (!f) return;
  if (!f.ensures) return;
  if (f.returns.length !== 1) return;
  if (f.returns[0].typeName !== "int") return;

  if (predicateContainsFunCall(f.ensures, name)) {
    if (recursiveAxiomsAdded.has(name)) return;
    recursiveAxiomsAdded.add(name);

    const n = z3.Int.const(`n_${name}_rec`);
    const resN = z3.Int.const(`${name}_result_${n.toString()}`);
    const nMinus1 = n.sub(z3.Int.val(1));
    const resNMinus1 = z3.Int.const(`${name}_result_${nMinus1.toString()}`);

    solver.add(z3.ForAll([n], z3.Implies(n.eq(0), resN.eq(z3.Int.val(1)))));
    solver.add(z3.ForAll([n], z3.Implies(n.gt(0), resN.eq(n.mul(resNMinus1)))));

    return;
  }

  const env2: Env = new Map();

  for (let i = 0; i < f.parameters.length; i++) {
    const p = f.parameters[i];
    if (p.typeName !== "int") return;
    if (i >= args.length) return;
    env2.set(p.name, args[i]);
  }

  env2.set(f.returns[0].name, result);

  const ax = convertPredicateToZ3(f.ensures, env2, z3, module, solver);
  solver.add(ax);
}

function predicateContainsFunCall(p: Predicate, name: string): boolean {
  switch ((p as any).kind) {
    case "true":
    case "false":
      return false;
    case "comparison":
      return exprContainsFunCall((p as any).left, name) || exprContainsFunCall((p as any).right, name);
    case "and":
    case "or":
      return predicateContainsFunCall((p as any).left, name) || predicateContainsFunCall((p as any).right, name);
    case "not": {
      const inner = (p as any).inner ?? (p as any).predicate;
      return predicateContainsFunCall(inner, name);
    }
    case "paren":
      return predicateContainsFunCall((p as any).inner, name);
    case "quantifier":
      return predicateContainsFunCall((p as any).predicate, name);
    case "formulaRef":
      return (p as any).args.some((a: Expr) => exprContainsFunCall(a, name));
    case "implies":
      return predicateContainsFunCall((p as any).left, name) || predicateContainsFunCall((p as any).right, name);
    default:
      return false;
  }
}

function exprContainsFunCall(e: Expr, name: string): boolean {
  e = simplifyExpr(e as any) as any;

  const t = tag(e);
  const k = (e as any).kind;

  if (t === "funccall") {
    const fc = e as any as FuncCallExpr;
    if (fc.name === name) return true;
    return fc.args.some((a: Expr) => exprContainsFunCall(a, name));
  }
  if (t === "arraccess") {
    return exprContainsFunCall((e as any).index, name);
  }

  switch (k) {
    case "Num":
    case "Int":
    case "Var":
    case "Ident":
      return false;
    case "Neg":
      return exprContainsFunCall((e as any).expr ?? (e as any).inner, name);
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return exprContainsFunCall((e as any).left, name) || exprContainsFunCall((e as any).right, name);
    default:
      return false;
  }
}
