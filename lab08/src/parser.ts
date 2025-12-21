import { getExprAst } from "../../lab04";
import * as ast from "./funny";
import { ErrorCode, FunnyError } from "./funny";

import grammar, { FunnyActionDict, FunnySemantics } from "./funny.ohm-bundle";

import { MatchResult, Semantics } from "ohm-js";
import * as arith from "../../lab04";

function collectList<T>(node: any): T[] {
  return node.asIteration().children.map((c: any) => c.parse() as T);
}

type FunEnv = Record<string, ast.FunctionDef>;
type VarEnv = Set<string>;

type PosInfo = {
  startLine?: number;
  startCol?: number;
  endLine?: number;
  endCol?: number;
};

let currentOrigin: string | undefined;

function intervalToLoc(interval: any): ast.SourceLoc {
  const src = interval?.source;
  const startIdx = interval?.startIdx ?? 0;
  const endIdx = interval?.endIdx ?? startIdx;

  const start =
    typeof src?.getLineAndColumn === "function"
      ? src.getLineAndColumn(startIdx)
      : { lineNum: 1, colNum: 1 };

  const end =
    typeof src?.getLineAndColumn === "function"
      ? src.getLineAndColumn(endIdx)
      : { lineNum: start.lineNum, colNum: start.colNum };

  return {
    file: currentOrigin,
    startLine: start.lineNum,
    startCol: start.colNum,
    endLine: end.lineNum,
    endCol: end.colNum,
  };
}

export function fail(code: ErrorCode, message: string, pos: PosInfo = {}): never {
  const { startLine, startCol, endLine, endCol } = pos;
  throw new FunnyError(message, code, startLine, startCol, endCol, endLine);
}

function declareVar(env: VarEnv, name: string, what: string): void {
  if (env.has(name)) {
    fail(ErrorCode.Redeclaration, `Redeclaration of ${what} "${name}".`);
  }
  env.add(name);
}

function ensureSingleValues(counts: number[], code: ErrorCode, message: string): void {
  if (counts.some((c) => c !== 1)) {
    fail(code, message);
  }
}

function ensureArgCount(name: string, expected: number, actual: number): void {
  if (actual !== expected) {
    fail(
      ErrorCode.ArgumentCount,
      `Argument count mismatch when calling "${name}": got ${actual}, expected ${expected}.`
    );
  }
}

function ensureDeclared(env: VarEnv, name: string, code: ErrorCode, messagePrefix: string): void {
  if (!env.has(name)) {
    fail(code, `${messagePrefix} "${name}".`);
  }
}

export function checkModule(mod: ast.Module): void {
  const funEnv: FunEnv = Object.create(null);

  for (const fn of mod.functions) {
    if (funEnv[fn.name]) {
      fail(ErrorCode.DuplicateFunction, `Duplicate function "${fn.name}".`);
    }
    funEnv[fn.name] = fn;
  }

  for (const fn of mod.functions) {
    checkFunction(fn, funEnv);
  }
}

function checkFunction(fn: ast.FunctionDef, funEnv: FunEnv): void {
  const env: VarEnv = new Set<string>();

  for (const p of fn.parameters) declareVar(env, p.name, "parameter");
  for (const r of fn.returns) declareVar(env, r.name, "return value");
  for (const l of fn.locals) declareVar(env, l.name, "local variable");

  checkStmt(fn.body, env, funEnv);
}

function checkStmt(stmt: ast.Statement, env: VarEnv, funEnv: FunEnv): void {
  switch (stmt.type) {
    case "assign": {
      for (const lv of stmt.targets) checkLValue(lv, env, funEnv);

      let produced = 0;
      for (const ex of stmt.exprs) produced += checkExpr(ex, env, funEnv);

      const needed = stmt.targets.length;
      if (produced !== needed) {
        fail(
          ErrorCode.AssignArity,
          `Assignment arity mismatch: ${needed} target(s) but ${produced} value(s) on right-hand side.`
        );
      }
      return;
    }

    case "block":
      for (const s of stmt.stmts) checkStmt(s, env, funEnv);
      return;

    case "if":
      checkCondition(stmt.condition, env, funEnv);
      checkStmt(stmt.then, env, funEnv);
      if (stmt.else) checkStmt(stmt.else, env, funEnv);
      return;

    case "while":
      checkCondition(stmt.condition, env, funEnv);
      checkStmt(stmt.body, env, funEnv);
      return;

    case "expr":
      checkExpr(stmt.expr, env, funEnv);
      return;
  }
}

function checkLValue(lv: ast.LValue, env: VarEnv, funEnv: FunEnv): void {
  switch (lv.type) {
    case "lvar":
      ensureDeclared(env, lv.name, ErrorCode.AssignUndeclaredVar, "Assignment to undeclared variable");
      return;

    case "larr":
      ensureDeclared(env, lv.name, ErrorCode.AssignUndeclaredArray, "Assignment to undeclared array");
      checkExpr(lv.index, env, funEnv);
      return;
  }
}

function checkFuncCall(call: ast.FuncCallExpr, env: VarEnv, funEnv: FunEnv): number {
  const { name, args } = call;

  // встроенная length(a: int[]) returns int
  if (name === "length") {
    ensureArgCount("length", 1, args.length);

    const argCount = checkExpr(args[0], env, funEnv);
    ensureSingleValues([argCount], ErrorCode.ArgumentMultiValue, "Function arguments must be single-valued.");

    return 1;
  }

  const fn = funEnv[name];
  if (!fn) {
    fail(ErrorCode.UnknownFunction, `Call to unknown function "${name}".`);
  }

  ensureArgCount(name, fn.parameters.length, args.length);

  for (const a of args) {
    const c = checkExpr(a, env, funEnv);
    ensureSingleValues([c], ErrorCode.ArgumentMultiValue, "Function arguments must be single-valued.");
  }

  return fn.returns.length;
}

function isFuncCallExpr(e: ast.Expr): e is ast.FuncCallExpr {
  return (e as any).type === "funccall";
}

function isArrAccessExpr(e: ast.Expr): e is ast.ArrAccessExpr {
  return (e as any).type === "arraccess";
}

// Возвращает число значений, производимых выражением
function checkExpr(e: ast.Expr, env: VarEnv, funEnv: FunEnv): number {
  if (isFuncCallExpr(e)) {
    return checkFuncCall(e, env, funEnv);
  }

  if (isArrAccessExpr(e)) {
    ensureDeclared(env, e.name, ErrorCode.AccessUndeclaredArray, "Access to undeclared array");
    const idxCount = checkExpr(e.index, env, funEnv);
    ensureSingleValues([idxCount], ErrorCode.ArrayIndexMultiValue, "Array index expression must produce exactly one value.");
    return 1;
  }

  // иначе — арифм.выражения из lab04
  const ae = e as arith.Expr;

  switch (ae.kind) {
    case "Int":
      return 1;

    case "Ident":
      ensureDeclared(env, ae.name, ErrorCode.UseUndeclaredVar, "Use of undeclared variable");
      return 1;

    case "Neg":
      return checkExpr(ae.expr as ast.Expr, env, funEnv);

    case "Add":
    case "Sub":
    case "Mul":
    case "Div": {
      const lCount = checkExpr(ae.left as ast.Expr, env, funEnv);
      const rCount = checkExpr(ae.right as ast.Expr, env, funEnv);
      ensureSingleValues([lCount, rCount], ErrorCode.OperatorMultiValue, "Operators can only be applied to single-valued expressions.");
      return 1;
    }

    default:
      return 1;
  }
}

function checkCondition(cond: ast.Condition, env: VarEnv, funEnv: FunEnv): void {
  switch (cond.kind) {
    case "true":
    case "false":
      return;

    case "comparison": {
      const lCount = checkExpr(cond.left, env, funEnv);
      const rCount = checkExpr(cond.right, env, funEnv);
      ensureSingleValues([lCount, rCount], ErrorCode.ComparisonMultiValue, "Comparison operands must be single-valued.");
      return;
    }

    case "not":
      checkCondition(cond.condition, env, funEnv);
      return;

    case "and":
    case "or":
      checkCondition(cond.left, env, funEnv);
      checkCondition(cond.right, env, funEnv);
      return;

    case "paren":
      checkCondition(cond.inner, env, funEnv);
      return;
  }
}

function foldLogicalChain<T>(first: any, rest: any, makeNode: (left: T, right: T) => T): T {
  let node = first.parse() as T;
  const restChildren = rest.children ?? rest.asIteration?.().children ?? [];
  for (const r of restChildren) {
    const rhs = r.parse() as T;
    node = makeNode(node, rhs);
  }
  return node;
}

function repeatPrefix<T>(nots: any, base: any, makeNode: (inner: T) => T): T {
  let node = base.parse() as T;
  const count = nots.children?.length ?? nots.asIteration?.().children.length ?? 0;
  for (let i = 0; i < count; i++) {
    node = makeNode(node);
  }
  return node;
}

function makeComparisonNode(leftNode: any, rightNode: any, op: ast.ComparisonCond["op"], loc?: ast.SourceLoc): ast.ComparisonCond {
  return {
    kind: "comparison",
    left: leftNode.parse() as ast.Expr,
    op,
    right: rightNode.parse() as ast.Expr,
    loc,
  };
}

export const getFunnyAst: FunnyActionDict<any> = {
  ...(getExprAst as any),

  Module(funcs: any) {
    const loc = intervalToLoc(this.source);
    const functions = funcs.children.map((f: any) => f.parse() as ast.FunctionDef);
    return { type: "module", functions, loc } as ast.Module;
  },

  Function(name: any, _lp: any, params: any, _rp: any, retSpec: any, usesOpt: any, block: any) {
    const loc = intervalToLoc(this.source);
    const locals = usesOpt.children.length > 0 ? (usesOpt.child(0).parse() as ast.ParameterDef[]) : [];
    return {
      type: "fun",
      name: name.sourceString,
      parameters: params.parse() as ast.ParameterDef[],
      returns: retSpec.parse() as ast.ParameterDef[],
      locals,
      body: block.parse() as ast.Statement,
      loc,
    } as ast.FunctionDef;
  },

  UsesSpec(_uses: any, params: any) {
    return params.parse() as ast.ParameterDef[];
  },

  RetSpec_multi(_returns: any, params: any) {
    return params.parse() as ast.ParameterDef[];
  },

  RetSpec_single(_returns: any, name: any, _colon: any, type: any) {
    const typeName = type.parse() as ast.ParameterDef["typeName"];
    return [
      {
        type: "param",
        name: name.sourceString,
        typeName,
        loc: intervalToLoc(this.source),
      } as ast.ParameterDef,
    ];
  },

  ParamList(list: any) {
    return collectList<ast.ParameterDef>(list);
  },

  ParamListNonEmpty(list: any) {
    return collectList<ast.ParameterDef>(list);
  },

  Param(name: any, _colon: any, type: any) {
    const typeName = type.parse() as ast.ParameterDef["typeName"];
    return {
      type: "param",
      name: name.sourceString,
      typeName,
      loc: intervalToLoc(this.source),
    } as ast.ParameterDef;
  },

  Type_array(_int: any, _arr: any) {
    return "int[]" as const;
  },

  Type_scalar(_int: any) {
    return "int" as const;
  },

  ArgList(list: any) {
    return collectList<ast.Expr>(list);
  },

  Block(_lb: any, stmts: any, _rb: any) {
    const loc = intervalToLoc(this.source);
    return {
      type: "block",
      stmts: stmts.children.map((s: any) => s.parse() as ast.Statement),
      loc,
    } as ast.BlockStmt;
  },

  Stmt(child: any) {
    return child.parse() as ast.Statement;
  },

  Stmt_expressionStatement(expr: any, _semi: any) {
    const loc = intervalToLoc(this.source);
    return {
      type: "expr",
      expr: expr.parse() as ast.Expr,
      loc,
    } as ast.ExprStmt;
  },

  While(_while: any, _lp: any, cond: any, _rp: any, body: any) {
    const loc = intervalToLoc(this.source);
    return {
      type: "while",
      condition: cond.parse() as ast.Condition,
      body: body.parse() as ast.Statement,
      loc,
    } as ast.WhileStmt;
  },

  If(_if: any, _lp: any, cond: any, _rp: any, thenStmt: any, _elseTok: any, elseStmtOpt: any) {
    const loc = intervalToLoc(this.source);
    let elseBranch: ast.Statement | null = null;
    if (elseStmtOpt.children.length > 0) {
      elseBranch = elseStmtOpt.child(0).parse() as ast.Statement;
    }
    return {
      type: "if",
      condition: cond.parse() as ast.Condition,
      then: thenStmt.parse() as ast.Statement,
      else: elseBranch,
      loc,
    } as ast.IfStmt;
  },

  Assign_tupleAssign(lvalues: any, _eq: any, exprs: any, _semi: any) {
    const loc = intervalToLoc(this.source);
    return {
      type: "assign",
      targets: lvalues.parse() as ast.LValue[],
      exprs: exprs.parse() as ast.Expr[],
      loc,
    } as ast.AssignStmt;
  },

  Assign_simpleAssign(lvalue: any, _eq: any, expr: any, _semi: any) {
    const loc = intervalToLoc(this.source);
    return {
      type: "assign",
      targets: [lvalue.parse() as ast.LValue],
      exprs: [expr.parse() as ast.Expr],
      loc,
    } as ast.AssignStmt;
  },

  LValueList(list: any) {
    return collectList<ast.LValue>(list);
  },

  ExprList(list: any) {
    return collectList<ast.Expr>(list);
  },

  LValue(child: any) {
    const loc = intervalToLoc(this.source);

    if (child.ctorName === "ArrayAccess") {
      const access = child.parse() as ast.ArrAccessExpr;
      return {
        type: "larr",
        name: access.name,
        index: access.index,
        loc,
      } as ast.ArrLValue;
    } else {
      return {
        type: "lvar",
        name: child.sourceString,
        loc,
      } as ast.VarLValue;
    }
  },

  FunctionCall(name: any, _lp: any, argsNode: any, _rp: any) {
    const loc = intervalToLoc(this.source);
    return {
      type: "funccall",
      name: name.sourceString,
      args: argsNode.parse() as ast.Expr[],
      loc,
    } as ast.FuncCallExpr;
  },

  ArrayAccess(name: any, _lb: any, index: any, _rb: any) {
    const loc = intervalToLoc(this.source);
    return {
      type: "arraccess",
      name: name.sourceString,
      index: index.parse() as ast.Expr,
      loc,
    } as ast.ArrAccessExpr;
  },

  Condition(orCond: any) {
    return orCond.parse() as ast.Condition;
  },

  OrCond(first: any, _ops: any, rest: any) {
    return foldLogicalChain<ast.Condition>(first, rest, (left, right) => ({
      kind: "or",
      left,
      right,
      loc: intervalToLoc(this.source),
    })) as any;
  },

  AndCond(first: any, _ops: any, rest: any) {
    return foldLogicalChain<ast.Condition>(first, rest, (left, right) => ({
      kind: "and",
      left,
      right,
      loc: intervalToLoc(this.source),
    })) as any;
  },

  NotCond(nots: any, atom: any) {
    let node = atom.parse() as ast.Condition;
    const count = nots.children?.length ?? nots.asIteration?.().children.length ?? 0;
    for (let i = 0; i < count; i++) {
      node = { kind: "not", condition: node, loc: intervalToLoc(this.source) } as ast.NotCond;
    }
    return node;
  },

  AtomCond_true(_t: any) {
    return { kind: "true", loc: intervalToLoc(this.source) } as ast.TrueCond;
  },

  AtomCond_false(_f: any) {
    return { kind: "false", loc: intervalToLoc(this.source) } as ast.FalseCond;
  },

  AtomCond_cmp(comp: any) {
    return comp.parse() as ast.ComparisonCond;
  },

  ParenCond(_lp: any, cond: any, _rp: any) {
    const loc = intervalToLoc(this.source);
    return { kind: "paren", inner: cond.parse() as ast.Condition, loc } as ast.ParenCond;
  },

  AtomCond_paren(parenNode: any) {
    return parenNode.parse() as ast.ParenCond;
  },

  Comparison(left: any, op: any, right: any) {
    const loc = intervalToLoc(this.source);
    const opStr = op.sourceString as ast.ComparisonCond["op"];
    return makeComparisonNode(left, right, opStr, loc);
  },
};

export const semantics: FunnySemanticsExt = grammar.Funny.createSemantics() as FunnySemanticsExt;
// ✅ фикс типизации ohm action dict
semantics.addOperation("parse()", getFunnyAst as any);

export interface FunnySemanticsExt extends FunnySemantics {
  (match: MatchResult): FunnyActionsExt;
}
interface FunnyActionsExt {
  parse(): ast.Module;
}

// ✅ Сигнатура не ломает старый код: origin необязателен
export function parseFunny(source: string, origin?: string): ast.Module {
  currentOrigin = origin;

  const match: MatchResult = grammar.Funny.match(source, "Module");

  if (match.failed()) {
    const m: any = match;
    const pos = typeof m.getRightmostFailurePosition === "function" ? m.getRightmostFailurePosition() : null;
    const message: string = m.message ?? "Syntax error in Funny module.";

    currentOrigin = undefined;
    fail(ErrorCode.ParseError, message, { startLine: pos?.lineNum, startCol: pos?.colNum });
  }

  const mod = (semantics as FunnySemanticsExt)(match).parse();
  checkModule(mod);
  currentOrigin = undefined;
  return mod;
}
