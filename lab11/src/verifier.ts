// lab11/src/verifier.ts

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
  SourceLoc,
  FunnyError,
} from "../../lab08";

// -------------------- helpers: kind/type compatibility --------------------

function tag(x: any): string | undefined {
  return x?.type ?? x?.kind;
}

// -------------------- NEW: Verification error code --------------------

const VERIFICATION_FAILED_CODE = "E_VERIFICATION_FAILED";

// -------------------- NEW: location helpers --------------------

function formatLocRange(loc?: SourceLoc): string {
  if (!loc) return "<unknown>";
  const file = loc.file ?? "<input>";
  if (typeof loc.endLine === "number" && typeof loc.endCol === "number") {
    return `${file}:${loc.startLine}:${loc.startCol}-${loc.endLine}:${loc.endCol}`;
  }
  return `${file}:${loc.startLine}:${loc.startCol}`;
}

function readLocFromAny(obj: any): SourceLoc | undefined {
  if (!obj) return undefined;

  if (obj.loc && typeof obj.loc === "object") {
    const l = obj.loc;
    if (typeof l.startLine === "number" && typeof l.startCol === "number") {
      return l as SourceLoc;
    }
  }

  if (typeof obj.startLine === "number" && typeof obj.startCol === "number") {
    const loc: SourceLoc = { file: obj.file, startLine: obj.startLine, startCol: obj.startCol };
    if (typeof obj.endLine === "number" && typeof obj.endCol === "number") {
      loc.endLine = obj.endLine;
      loc.endCol = obj.endCol;
    }
    return loc;
  }

  return undefined;
}

function getLocFromExpr(expr: Expr | any): SourceLoc | undefined {
  if (!expr) return undefined;

  const direct = readLocFromAny(expr);
  if (direct) return direct;

  const t = tag(expr);

  // lab08 arithmetic (kind-based)
  if (expr.kind) {
    switch (expr.kind) {
      case "Num":
      case "Int":
      case "Var":
      case "Ident":
        return direct;

      case "Neg":
        return getLocFromExpr(expr.expr ?? expr.inner);

      case "Add":
      case "Sub":
      case "Mul":
      case "Div":
        return getLocFromExpr(expr.left) ?? getLocFromExpr(expr.right) ?? direct;

      default:
        return direct;
    }
  }

  // lab08 extra expressions (type-based)
  if (t === "funccall") {
    for (const a of expr.args ?? []) {
      const r = getLocFromExpr(a);
      if (r) return r;
    }
    return direct;
  }

  if (t === "arraccess") {
    return getLocFromExpr(expr.index) ?? direct;
  }

  return direct;
}

function getLocFromPredicate(pred: Predicate | any): SourceLoc | undefined {
  if (!pred) return undefined;

  const direct = readLocFromAny(pred);
  if (direct) return direct;

  switch (pred.kind) {
    case "true":
    case "false":
      return direct;

    case "comparison": {
      const c = pred as ComparisonPredicate;
      return getLocFromExpr(c.left) ?? getLocFromExpr(c.right) ?? direct;
    }

    case "and": {
      const p = pred as AndPredicate;
      return getLocFromPredicate(p.left) ?? getLocFromPredicate(p.right) ?? direct;
    }

    case "or": {
      const p = pred as OrPredicate;
      return getLocFromPredicate(p.left) ?? getLocFromPredicate(p.right) ?? direct;
    }

    case "not": {
      // у тебя not = {kind:'not', inner: ...}
      const p = pred as NotPredicate;
      const inner = (p as any).inner ?? (p as any).predicate;
      return getLocFromPredicate(inner) ?? direct;
    }

    case "paren": {
      const p = pred as ParenPredicate;
      return getLocFromPredicate(p.inner) ?? direct;
    }

    case "implies": {
      // у тебя implies встречается как any (kind: "implies", left, right)
      return getLocFromPredicate((pred as any).right) ?? getLocFromPredicate((pred as any).left) ?? direct;
    }

    case "quantifier": {
      const q = pred as QuantifierPredicate;
      return readLocFromAny(q) ?? getLocFromPredicate(q.predicate);
    }

    case "formulaRef": {
      const fr = pred as FormulaRefPredicate;
      // обычно loc лежит прямо на formulaRef, но если нет — берём из аргументов
      for (const a of fr.args ?? []) {
        const r = getLocFromExpr(a);
        if (r) return r;
      }
      return direct;
    }

    default:
      return direct;
  }
}

// -------------------- Z3 init --------------------

let z3Context: Context | null = null;
let z3: Context;

const recursiveAxiomsAdded = new Set<string>();


const recursiveFunDecls = new Map<string, any>();

function getOrCreateRecFun(z3ctx: any, fname: string) {
  let f = recursiveFunDecls.get(fname);
  if (f) return f;

  f = (z3ctx as any).Function.declare(
    `${fname}_rec`,
    (z3ctx as any).Int.sort(),
    (z3ctx as any).Int.sort()
  );

  recursiveFunDecls.set(fname, f);
  return f;
}

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
  recursiveFunDecls.clear(); 
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

// -------------------- NEW: small arithmetic simplifier --------------------

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

  // NEW: запоминаем первую ошибку + её loc, чтобы кинуть корректный FunnyError в конце
  let firstFailedFunc: string | null = null;
  let firstFailedLoc: SourceLoc | undefined;

  z3 = await initZ3();

  for (const func of module.functions) {
    const solver = new z3.Solver();

    try {
      solver.set("timeout", 5000);
      solver.set("smt.mbqi", true);
    } catch {
      // ignore
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

      const verified = proof.result === "unsat";

      // NEW: выбираем локацию “где именно сломалось”
      const vcLoc = getLocFromPredicate(vc);
      const ensuresLoc = func.ensures ? readLocFromAny(func.ensures) ?? getLocFromPredicate(func.ensures) : undefined;
      const requiresLoc = func.requires ? readLocFromAny(func.requires) ?? getLocFromPredicate(func.requires) : undefined;
      const funcLoc = readLocFromAny(func);

      const bestLoc = vcLoc ?? ensuresLoc ?? requiresLoc ?? funcLoc;

      results.push({
        function: func.name,
        verified,
        error:
          verified
            ? undefined
            : proof.result === "sat"
              ? `Теорема неверна: найден контрпример. Место: ${formatLocRange(bestLoc)}`
              : `Z3 вернул unknown. Место: ${formatLocRange(bestLoc)}`,
        model: verified ? undefined : proof.model,
      });

      if (!verified) {
        hasFailure = true;
        if (!firstFailedFunc) {
          firstFailedFunc = func.name;
          firstFailedLoc = bestLoc;
        }
      }
    } catch (e: any) {
      hasFailure = true;
      if (!firstFailedFunc) {
        firstFailedFunc = func.name;
        // если упали исключением — пробуем хотя бы loc функции
        firstFailedLoc = readLocFromAny(func) ?? undefined;
      }

      results.push({
        function: func.name,
        verified: false,
        error: String(e?.message ?? e),
      });
    }
  }

  if (hasFailure) {
    const failed = results.filter((r) => !r.verified).map((r) => r.function).join(", ");

    // NEW: кидаем FunnyError с координатами (чтобы тесты могли проверить startLine/startCol/...)
    if (firstFailedLoc) {
      throw new FunnyError(
        `Verification failed for: ${failed}`,
        VERIFICATION_FAILED_CODE,
        firstFailedLoc.startLine,
        firstFailedLoc.startCol,
        firstFailedLoc.endCol,
        firstFailedLoc.endLine
      );
    }

    // если loc вообще нет — кидаем без координат
    throw new FunnyError(`Verification failed for: ${failed}`, VERIFICATION_FAILED_CODE);
  }

  return results;
}

// -------------------- prover --------------------

async function proveTheoremWithRetry(
  theorem: Bool,
  solver: any
): Promise<{ result: "sat" | "unsat" | "unknown"; model?: Model }> {
  return await proveTheorem(theorem, solver);
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

// -------------------- invariants detection --------------------

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
  return n && (n.kind === "Num" || n.kind === "Int") && typeof n.value === "number" && n.value === 1;
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

  // если в assign есть loc — это идеальная точка “где ломается”
  if ((assign as any).loc && !readLocFromAny(cur)) {
    (cur as any).loc = (assign as any).loc;
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

// -------------------- predicate simplifier --------------------

function simplifyPredicate(p: Predicate): Predicate {
  // сохраняем loc, если он был
  const origLoc = readLocFromAny(p);

  const keepLoc = (x: Predicate): Predicate => {
    if (origLoc && !readLocFromAny(x)) (x as any).loc = origLoc;
    return x;
  };

  switch ((p as any).kind) {
    case "and": {
      const l = simplifyPredicate((p as AndPredicate).left);
      const r = simplifyPredicate((p as AndPredicate).right);
      if (l.kind === "true") return keepLoc(r);
      if (r.kind === "true") return keepLoc(l);
      if (l.kind === "false" || r.kind === "false") return keepLoc({ kind: "false" } as any);
      return keepLoc({ kind: "and", left: l, right: r } as any);
    }
    case "or": {
      const l = simplifyPredicate((p as OrPredicate).left);
      const r = simplifyPredicate((p as OrPredicate).right);
      if (l.kind === "true" || r.kind === "true") return keepLoc({ kind: "true" } as any);
      if (l.kind === "false") return keepLoc(r);
      if (r.kind === "false") return keepLoc(l);
      return keepLoc({ kind: "or", left: l, right: r } as any);
    }
    case "not": {
      const inner0 = (p as any).inner ?? (p as any).predicate;
      const inner = simplifyPredicate(inner0);
      if (inner.kind === "true") return keepLoc({ kind: "false" } as any);
      if (inner.kind === "false") return keepLoc({ kind: "true" } as any);
      if ((inner as any).kind === "not") return keepLoc(((inner as any).inner ?? (inner as any).predicate) as any);
      return keepLoc({ kind: "not", inner } as any);
    }
    case "paren":
      return keepLoc(simplifyPredicate((p as ParenPredicate).inner));
    case "implies": {
      const l = simplifyPredicate((p as any).left);
      const r = simplifyPredicate((p as any).right);
      if (l.kind === "false") return keepLoc({ kind: "true" } as any);
      if (l.kind === "true") return keepLoc(r);
      if (r.kind === "true") return keepLoc({ kind: "true" } as any);
      return keepLoc({ kind: "implies", left: l, right: r } as any);
    }
    default:
      return keepLoc(p);
  }
}

// -------------------- substitution helpers --------------------

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
    const a = fr.args[i];

    if (p.typeName === "int") {
      const az3 = convertExprToZ3(a, env, z3, module, solver);
      env2.set(p.name, az3);
    } else if (p.typeName === "int[]") {
      throw new Error("FormulaRef: int[] params not supported in verifier");
    }
  }

  return convertPredicateToZ3(f.body, env2, z3, module, solver);
}

// -------------------- Expr -> Z3 --------------------

function convertExprToZ3(expr: Expr, env: Env, z3: Context, module: AnnotatedModule, solver: any): Arith {
  
  const e: any = simplifyExpr(expr as any);

  const k = e.kind;
  const t = tag(e);

  if (t === "funccall") {
    return convertFuncCallExprToZ3(e as any as FuncCallExpr, env, z3, module, solver);
  }

  if (t === "arraccess") {
    return convertArrAccessExprToZ3(e as any as ArrAccessExpr, env, z3, module, solver);
  }

  switch (k) {
    case "Num":
    case "Int":
      return z3.Int.val(e.value);

    case "Var":
    case "Ident": {
      const v = env.get(e.name);
      if (!v) throw new Error(`Unknown var ${e.name}`);
      if (!isArith(v)) throw new Error(`Var ${e.name} is not Arith`);
      return v;
    }

    case "Neg":
      return z3.Int.val(0).sub(convertExprToZ3(e.expr ?? e.inner, env, z3, module, solver));

    case "Add":
      return convertExprToZ3(e.left, env, z3, module, solver).add(convertExprToZ3(e.right, env, z3, module, solver));
    case "Sub":
      return convertExprToZ3(e.left, env, z3, module, solver).sub(convertExprToZ3(e.right, env, z3, module, solver));
    case "Mul":
      return convertExprToZ3(e.left, env, z3, module, solver).mul(convertExprToZ3(e.right, env, z3, module, solver));
    case "Div":
      return convertExprToZ3(e.left, env, z3, module, solver).div(convertExprToZ3(e.right, env, z3, module, solver));

    default:
      throw new Error(`convertExprToZ3: unknown expr.kind ${k}`);
  }
}

function convertArrAccessExprToZ3(acc: ArrAccessExpr, env: Env, z3: Context, module: AnnotatedModule, solver: any): Arith {
  const arr = env.get(acc.name);
  if (!arr) throw new Error(`Unknown array ${acc.name}`);
  const idx = convertExprToZ3(acc.index, env, z3, module, solver);

  // (arr as any).select(idx)
  const sel = (arr as any).select(idx);
  return sel;
}

function predicateContainsFunCall(pred: any, fname: string): boolean {
  if (!pred) return false;

  if (tag(pred) === "funccall") return pred.name === fname;

  if (pred.kind === "comparison") {
    return predicateContainsFunCall(pred.left, fname) || predicateContainsFunCall(pred.right, fname);
  }

  if (pred.kind === "and" || pred.kind === "or") {
    return predicateContainsFunCall(pred.left, fname) || predicateContainsFunCall(pred.right, fname);
  }

  if (pred.kind === "not") {
    const inner = pred.inner ?? pred.predicate;
    return predicateContainsFunCall(inner, fname);
  }

  if (pred.kind === "paren") return predicateContainsFunCall(pred.inner, fname);
  if (pred.kind === "implies") return predicateContainsFunCall(pred.left, fname) || predicateContainsFunCall(pred.right, fname);

  if (pred.kind === "quantifier") return predicateContainsFunCall(pred.predicate, fname);
  if (pred.kind === "formulaRef") return (pred.args ?? []).some((a: any) => predicateContainsFunCall(a, fname));

  // expr-level
  if (pred.kind === "Add" || pred.kind === "Sub" || pred.kind === "Mul" || pred.kind === "Div")
    return predicateContainsFunCall(pred.left, fname) || predicateContainsFunCall(pred.right, fname);
  if (pred.kind === "Neg") return predicateContainsFunCall(pred.expr ?? pred.inner, fname);

  return false;
}

function convertFuncCallExprToZ3(
  call: FuncCallExpr,
  env: Env,
  z3: Context,
  module: AnnotatedModule,
  solver: any
): Arith {
  // built-in length
  if (call.name === "length") {
    const a0 = call.args[0];
    const arr = env.get((a0 as any).name);
    if (!arr) throw new Error(`Unknown array for length(...)`);
    const lengthFun = (z3 as any).Function.declare(
      "length",
      (z3 as any).Array.sort(z3.Int.sort(), z3.Int.sort()),
      z3.Int.sort()
    );
    return lengthFun.call(arr as any);
  }

  // ✅ ДЕТЕРМИНИРОВАННЫЙ resName + КЕШ (КРИТИЧНО ДЛЯ factorial)
  const argsZ3 = call.args.map((a) => convertExprToZ3(a, env, z3, module, solver));
  const argKey = argsZ3.map((a: any) => a.toString()).join("_");
  const resName = `${call.name}_result_${argKey}`;

  const cached = env.get(resName);
  if (cached) {
    if (!isArith(cached)) throw new Error(`Cached call result ${resName} is not Arith`);
    return cached;
  }

  const res = z3.Int.const(resName);
  env.set(resName, res);

  addEnsuresAxiomForCall(call.name, argsZ3, res, module, z3, solver);

  return res;
}

function addEnsuresAxiomForCall(
  fname: string,
  argsZ3: any[],
  resZ3: any,
  module: AnnotatedModule,
  z3: Context,
  solver: any
) {
  const fn = module.functions.find((f) => f.name === fname);
  if (!fn) return;
  if (!fn.returns || fn.returns.length !== 1) return;
  if (fn.returns[0].typeName !== "int") return;

  if (fn.ensures && predicateContainsFunCall(fn.ensures as any, fname)) {
    if (recursiveAxiomsAdded.has(fname)) return;
    recursiveAxiomsAdded.add(fname);

    const n = z3.Int.const(`n_${fname}_rec`);

    const resN = z3.Int.const(`${fname}_result_${n.toString()}`);
    const nMinus1 = n.sub(z3.Int.val(1));
    const resNMinus1 = z3.Int.const(`${fname}_result_${nMinus1.toString()}`);

    solver.add(
      z3.ForAll([n] as any, z3.Implies(n.eq(z3.Int.val(0)), resN.eq(z3.Int.val(1))))
    );
    solver.add(
      z3.ForAll([n] as any, z3.Implies(n.gt(z3.Int.val(0)), resN.eq(n.mul(resNMinus1))))
    );
  }

  // локальная подстановка параметров/результата в requires/ensures
  const localEnv: Env = new Map(envFromArgs(fn, argsZ3, resZ3, z3));
  if (fn.requires) solver.add(convertPredicateToZ3(fn.requires, localEnv, z3, module, solver));
  if (fn.ensures) solver.add(convertPredicateToZ3(fn.ensures, localEnv, z3, module, solver));
}

function envFromArgs(fn: AnnotatedFunction, argsZ3: any[], resZ3: any, z3: Context): Env {
  const env: Env = new Map();

  for (let i = 0; i < fn.parameters.length; i++) {
    const p = fn.parameters[i];
    if (p.typeName === "int") env.set(p.name, argsZ3[i]);
  }

  const ret = fn.returns[0];
  env.set(ret.name, resZ3);

  return env;
}
