import { getExprAst } from '../../lab04';
import * as ast from './funny';
import { ErrorCode, FunnyError } from './funny';

import grammar, { FunnyActionDict, FunnySemantics } from './funny.ohm-bundle';


import { MatchResult, Semantics } from 'ohm-js';
import * as arith from '../../lab04';
// ============================
// ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
// ============================

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

export function fail(code: ErrorCode, message: string, pos: PosInfo = {}): never {
    const { startLine, startCol, endLine, endCol } = pos;
    throw new FunnyError(message, code, startLine, startCol, endCol, endLine);
}

function declareVar(env: VarEnv, name: string, what: string): void {
    if (env.has(name)) {
        fail(
            ErrorCode.Redeclaration,
            `Redeclaration of ${what} "${name}".`
        );
    }
    env.add(name);
}

function ensureSingleValues(
    counts: number[],
    code: ErrorCode,
    message: string
): void {
    if (counts.some((c) => c !== 1)) {
        fail(code, message);
    }
}

function ensureArgCount(
    name: string,
    expected: number,
    actual: number
): void {
    if (actual !== expected) {
        fail(
            ErrorCode.ArgumentCount,
            `Argument count mismatch when calling "${name}": got ${actual}, expected ${expected}.`
        );
    }
}

function ensureDeclared(
    env: VarEnv,
    name: string,
    code: ErrorCode,
    messagePrefix: string
): void {
    if (!env.has(name)) {
        fail(code, `${messagePrefix} "${name}".`);
    }
}

// ============================
// СЕМАНТИЧЕСКИЕ ПРОВЕРКИ
// ============================

export function checkModule(mod: ast.Module): void {
    const funEnv: FunEnv = Object.create(null);

    // 1) дубликаты функций
    for (const fn of mod.functions) {
        if (funEnv[fn.name]) {
            fail(ErrorCode.DuplicateFunction, `Duplicate function "${fn.name}".`);
        }
        funEnv[fn.name] = fn;
    }

    // 2) проверки внутри функций
    for (const fn of mod.functions) {
        checkFunction(fn, funEnv);
    }
}

function checkFunction(fn: ast.FunctionDef, funEnv: FunEnv): void {
    const env: VarEnv = new Set<string>();

    // параметры
    for (const p of fn.parameters) {
        declareVar(env, p.name, 'parameter');
    }

    // возвращаемые
    for (const r of fn.returns) {
        declareVar(env, r.name, 'return value');
    }

    // локальные
    for (const l of fn.locals) {
        declareVar(env, l.name, 'local variable');
    }

    checkStmt(fn.body, env, funEnv);
}

function checkStmt(stmt: ast.Statement, env: VarEnv, funEnv: FunEnv): void {
    switch (stmt.type) {
        case 'assign': {
            for (const lv of stmt.targets) {
                checkLValue(lv, env, funEnv);
            }

            let produced = 0;
            for (const ex of stmt.exprs) {
                produced += checkExpr(ex, env, funEnv);
            }
            const needed = stmt.targets.length;
            if (produced !== needed) {
                fail(
                    ErrorCode.AssignArity,
                    `Assignment arity mismatch: ${needed} target(s) but ${produced} value(s) on right-hand side.`
                );
            }
            return;
        }

        case 'block':
            for (const s of stmt.stmts) {
                checkStmt(s, env, funEnv);
            }
            return;

        case 'if':
            checkCondition(stmt.condition, env, funEnv);
            checkStmt(stmt.then, env, funEnv);
            if (stmt.else) {
                checkStmt(stmt.else, env, funEnv);
            }
            return;

        case 'while':
            checkCondition(stmt.condition, env, funEnv);
            checkStmt(stmt.body, env, funEnv);
            return;

        case 'expr':
            checkExpr(stmt.expr, env, funEnv);
            return;
    }
}

function checkLValue(lv: ast.LValue, env: VarEnv, funEnv: FunEnv): void {
    switch (lv.type) {
        case 'lvar':
            ensureDeclared(
                env,
                lv.name,
                ErrorCode.AssignUndeclaredVar,
                'Assignment to undeclared variable'
            );
            return;

        case 'larr':
            ensureDeclared(
                env,
                lv.name,
                ErrorCode.AssignUndeclaredArray,
                'Assignment to undeclared array'
            );
            checkExpr(lv.index, env, funEnv);
            return;
    }
}

function checkFuncCall(
    call: ast.FuncCallExpr,
    env: VarEnv,
    funEnv: FunEnv
): number {
    const { name, args } = call;

    // встроенная length(a: int[]) returns int
    if (name === 'length') {
        ensureArgCount('length', 1, args.length);

        const argCount = checkExpr(args[0], env, funEnv);
        ensureSingleValues(
            [argCount],
            ErrorCode.ArgumentMultiValue,
            'Function arguments must be single-valued.'
        );

        return 1;
    }

    const fn = funEnv[name];
    if (!fn) {
        fail(
            ErrorCode.UnknownFunction,
            `Call to unknown function "${name}".`
        );
    }

    ensureArgCount(name, fn.parameters.length, args.length);

    for (const a of args) {
        const c = checkExpr(a, env, funEnv);
        ensureSingleValues(
            [c],
            ErrorCode.ArgumentMultiValue,
            'Function arguments must be single-valued.'
        );
    }

    return fn.returns.length;
}

// различаем арифметический Expr (из lab04) и расширенные узлы
function isFuncCallExpr(e: ast.Expr): e is ast.FuncCallExpr {
    return (e as any).type === 'funccall';
}

function isArrAccessExpr(e: ast.Expr): e is ast.ArrAccessExpr {
    return (e as any).type === 'arraccess';
}

// проверка выражений: возвращает, сколько значений оно даёт (1 или >1)
function checkExpr(e: ast.Expr, env: VarEnv, funEnv: FunEnv): number {
    // наши расширенные выражения
    if (isFuncCallExpr(e)) {
        return checkFuncCall(e, env, funEnv);
    }
    if (isArrAccessExpr(e)) {
        ensureDeclared(
            env,
            e.name,
            ErrorCode.AccessUndeclaredArray,
            'Access to undeclared array'
        );
        const idxCount = checkExpr(e.index, env, funEnv);
        ensureSingleValues(
            [idxCount],
            ErrorCode.ArrayIndexMultiValue,
            'Array index expression must produce exactly one value.'
        );
        return 1;
    }

    // иначе — арифм.выражения из lab04
    const ae = e as arith.Expr;
    switch (ae.kind) {
        case 'Int':
            return 1;

        case 'Ident':
            ensureDeclared(
                env,
                ae.name,
                ErrorCode.UseUndeclaredVar,
                'Use of undeclared variable'
            );
            return 1;

        case 'Neg':
            return checkExpr(ae.expr as ast.Expr, env, funEnv);

        case 'Add':
        case 'Sub':
        case 'Mul':
        case 'Div': {
            const lCount = checkExpr(ae.left as ast.Expr, env, funEnv);
            const rCount = checkExpr(ae.right as ast.Expr, env, funEnv);
            ensureSingleValues(
                [lCount, rCount],
                ErrorCode.OperatorMultiValue,
                'Operators can only be applied to single-valued expressions.'
            );
            return 1;
        }
        default:
            return 1;
    }
}

// проверки условий if/while
function checkCondition(
    cond: ast.Condition,
    env: VarEnv,
    funEnv: FunEnv
): void {
    switch (cond.kind) {
        case 'true':
        case 'false':
            return;

        case 'comparison': {
            const lCount = checkExpr(cond.left, env, funEnv);
            const rCount = checkExpr(cond.right, env, funEnv);
            ensureSingleValues(
                [lCount, rCount],
                ErrorCode.ComparisonMultiValue,
                'Comparison operands must be single-valued.'
            );
            return;
        }

        case 'not':
            checkCondition(cond.condition, env, funEnv);
            return;

        case 'and':
        case 'or':
            checkCondition(cond.left, env, funEnv);
            checkCondition(cond.right, env, funEnv);
            return;

        case 'paren':
            checkCondition(cond.inner, env, funEnv);
            return;
    }
}

// утилита для цепочек And/Or
function foldLogicalChain<T>(
    first: any,
    rest: any,
    makeNode: (left: T, right: T) => T
): T {
    let node = first.parse() as T;
    const restChildren = rest.children ?? rest.asIteration?.().children ?? [];
    for (const r of restChildren) {
        const rhs = r.parse() as T;
        node = makeNode(node, rhs);
    }
    return node;
}

// утилита для повторяющихся not
function repeatPrefix<T>(
    nots: any,
    base: any,
    makeNode: (inner: T) => T
): T {
    let node = base.parse() as T;
    const count = nots.children?.length ?? nots.asIteration?.().children.length ?? 0;
    for (let i = 0; i < count; i++) {
        node = makeNode(node);
    }
    return node;
}

// Comparison: создаём AST ComparisonCond
function makeComparisonNode(
    leftNode: any,
    rightNode: any,
    op: ast.ComparisonCond['op']
): ast.ComparisonCond {
    return {
        kind: 'comparison',
        left: leftNode.parse() as ast.Expr,
        op,
        right: rightNode.parse() as ast.Expr,
    };
}

// ============================
// AST-построение из Ohm
// ============================

export const getFunnyAst: FunnyActionDict<any> = {
    // арифметические выражения из lab04
    ...(getExprAst as any),

    // ---------- корень ----------
    Module(funcs: any) {
        const functions = funcs.children.map(
            (f: any) => f.parse() as ast.FunctionDef
        );
        return {
            type: 'module',
            functions,
        } as ast.Module;
    },

    // Function = Ident "(" ParamList ")" RetSpec UsesSpec? Block
    Function(name: any, _lp: any, params: any, _rp: any, retSpec: any, usesOpt: any, block: any) {
        const locals =
            usesOpt.children.length > 0
                ? (usesOpt.child(0).parse() as ast.ParameterDef[])
                : [];
        return {
            type: 'fun',
            name: name.sourceString,
            parameters: params.parse() as ast.ParameterDef[],
            returns: retSpec.parse() as ast.ParameterDef[],
            locals,
            body: block.parse() as ast.Statement,
        } as ast.FunctionDef;
    },

    UsesSpec(_uses: any, params: any) {
        return params.parse() as ast.ParameterDef[];
    },

    // RetSpec
    RetSpec_multi(_returns: any, params: any) {
        return params.parse() as ast.ParameterDef[];
    },

    RetSpec_single(_returns: any, name: any, _colon: any, type: any) {
        const typeName = type.parse() as ast.ParameterDef['typeName'];
        return [
            {
                type: 'param',
                name: name.sourceString,
                typeName,
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
        const typeName = type.parse() as ast.ParameterDef['typeName'];
        return {
            type: 'param',
            name: name.sourceString,
            typeName,
        } as ast.ParameterDef;
    },

    // Type
    Type_array(_int: any, _arr: any) {
        return 'int[]' as const;
    },

    Type_scalar(_int: any) {
        return 'int' as const;
    },

    ArgList(list: any) {
        return collectList<ast.Expr>(list);
    },

    Block(_lb: any, stmts: any, _rb: any) {
        return {
            type: 'block',
            stmts: stmts.children.map((s: any) => s.parse() as ast.Statement),
        } as ast.BlockStmt;
    },

        // Stmt = Assign | Block | While | If | Expr ";" -- expressionStatement
    Stmt(child: any) {
        return child.parse() as ast.Statement;
    },

    Stmt_expressionStatement(expr: any, _semi: any) {
        return {
            type: 'expr',
            expr: expr.parse() as ast.Expr,
        } as ast.ExprStmt;
    },

    // While = "while" "(" Condition ")" Block
    While(_while: any, _lp: any, cond: any, _rp: any, body: any) {
        return {
            type: 'while',
            condition: cond.parse() as ast.Condition,
            body: body.parse() as ast.Statement,
        } as ast.WhileStmt;
    },

    // If = "if" "(" Condition ")" Stmt ("else" Stmt)?
    If(_if: any, _lp: any, cond: any, _rp: any, thenStmt: any, _elseTok: any, elseStmtOpt: any) {
        let elseBranch: ast.Statement | null = null;

        // elseStmtOpt — это Optional<Stmt>, у него либо 0, либо 1 ребёнок
        if (elseStmtOpt.children.length > 0) {
            elseBranch = elseStmtOpt.child(0).parse() as ast.Statement;
        }

        return {
            type: 'if',
            condition: cond.parse() as ast.Condition,
            then: thenStmt.parse() as ast.Statement,
            else: elseBranch,
        } as ast.IfStmt;
    },

    // Assign
    Assign_tupleAssign(lvalues: any, _eq: any, exprs: any, _semi: any) {
        return {
            type: 'assign',
            targets: lvalues.parse() as ast.LValue[],
            exprs: exprs.parse() as ast.Expr[],
        } as ast.AssignStmt;
    },

    Assign_simpleAssign(lvalue: any, _eq: any, expr: any, _semi: any) {
        return {
            type: 'assign',
            targets: [lvalue.parse() as ast.LValue],
            exprs: [expr.parse() as ast.Expr],
        } as ast.AssignStmt;
    },

    LValueList(list: any) {
        return collectList<ast.LValue>(list);
    },

    ExprList(list: any) {
        return collectList<ast.Expr>(list);
    },

    // LValue = ArrayAccess | Ident
    LValue(child: any) {
        if (child.ctorName === 'ArrayAccess') {
            const access = child.parse() as ast.ArrAccessExpr;
            return {
                type: 'larr',
                name: access.name,
                index: access.index,
            } as ast.ArrLValue;
        } else {
            // Ident
            return {
                type: 'lvar',
                name: child.sourceString,
            } as ast.VarLValue;
        }
    },

    FunctionCall(name: any, _lp: any, argsNode: any, _rp: any) {
        return {
            type: 'funccall',
            name: name.sourceString,
            args: argsNode.parse() as ast.Expr[],
        } as ast.FuncCallExpr;
    },

    ArrayAccess(name: any, _lb: any, index: any, _rb: any) {
        return {
            type: 'arraccess',
            name: name.sourceString,
            index: index.parse() as ast.Expr,
        } as ast.ArrAccessExpr;
    },

    Condition(orCond: any) {
        return orCond.parse() as ast.Condition;
    },

    OrCond(first: any, _ops: any, rest: any) {
        return foldLogicalChain<ast.Condition>(first, rest, (left, right) => ({
            kind: 'or',
            left,
            right,
        } as ast.OrCond));
    },

    AndCond(first: any, _ops: any, rest: any) {
        return foldLogicalChain<ast.Condition>(first, rest, (left, right) => ({
            kind: 'and',
            left,
            right,
        } as ast.AndCond));
    },

    NotCond(nots: any, atom: any) {
        return repeatPrefix<ast.Condition>(nots, atom, (condition) => ({
            kind: 'not',
            condition,
        } as ast.NotCond));
    },

    AtomCond_true(_t: any) {
        return { kind: 'true' } as ast.TrueCond;
    },

    AtomCond_false(_f: any) {
        return { kind: 'false' } as ast.FalseCond;
    },

    AtomCond_cmp(comp: any) {
        // Comparison уже вернёт ComparisonCond
        return comp.parse() as ast.ComparisonCond;
    },

    // ParenCond = "(" Condition ")"
    ParenCond(_lp: any, cond: any, _rp: any) {
        // вернём уже готовый ParenCond-узел
        return {
            kind: 'paren',
            inner: cond.parse() as ast.Condition,
        } as ast.ParenCond;
    },

    // AtomCond ... | ParenCond -- paren
    AtomCond_paren(parenNode: any) {
        // просто пробрасываем результат ParenCond
        return parenNode.parse() as ast.ParenCond;
    },

    // Comparison
    Comparison(left: any, op: any, right: any) {
        const opStr = op.sourceString as ast.ComparisonCond['op'];
        return makeComparisonNode(left, right, opStr);
    },
};

export const semantics: FunnySemanticsExt = grammar.Funny.createSemantics() as FunnySemanticsExt;
semantics.addOperation('parse()', getFunnyAst);

export interface FunnySemanticsExt extends FunnySemantics {
    (match: MatchResult): FunnyActionsExt;
}
interface FunnyActionsExt {
    parse(): ast.Module;
}

// ============================
// parseFunny
// ============================

export function parseFunny(source: string): ast.Module {
    const match: MatchResult = grammar.Funny.match(source, 'Module');

    if (match.failed()) {
        const m: any = match;
        const pos =
            typeof m.getRightmostFailurePosition === 'function'
                ? m.getRightmostFailurePosition()
                : null;

        const message: string =
            m.message ?? 'Syntax error in Funny module.';

        fail(ErrorCode.ParseError, message, {
            startLine: pos?.lineNum,
            startCol: pos?.colNum,
        });
    }

    const mod = (semantics as FunnySemanticsExt)(match).parse();
    checkModule(mod);
    return mod;
}
