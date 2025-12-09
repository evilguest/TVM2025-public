import { writeFileSync } from "fs";
import { Op, I32, Void, c, BufferedEmitter, LocalEntry} from "../../wasm";
import { Module } from "../../lab08";

const { i32, 
    varuint32,
    get_local, local_entry, set_local, call, if_, void_block, void_loop, br_if, str_ascii, export_entry,
    func_type_m, function_body, type_section, function_section, export_section, code_section } = c;
  
export async function compileModule<M extends Module>(m: M, name?: string): Promise<WebAssembly.Exports>
{
    const i64 = (c as any).i64;
    const array_get   = (c as any).array_get;
    const array_set   = (c as any).array_set;
    const check_bounds = (c as any).check_bounds;

    type FunnyModule = Module & { functions: any[] };

    type FunnyFunction = {
        type: "fun";
        name: string;
        parameters: { name: string; typeName: "int" | "int[]" }[];
        returns: { name: string; typeName: "int" | "int[]" }[];
        locals: { name: string; typeName: "int" | "int[]" }[];
        body: any;
    };

    type VarInfo = { index: number; isArray: boolean };
    type VarIndexMap = Record<string, VarInfo>;
    type FunIndexMap = Record<string, number>;
    type FunInfoMap = Record<string, FunnyFunction>;

    type Expr = any;
    type Condition = any;
    type Stmt = any;

    interface FunCompileContext {
        funIndices: FunIndexMap;
        funInfos: FunInfoMap;
        vars: VarIndexMap;
    }

    const mod = m as unknown as FunnyModule;
    const funs = mod.functions as FunnyFunction[];

    const funIndices: FunIndexMap = Object.create(null);
    const funInfos: FunInfoMap = Object.create(null);
    funs.forEach((fn, i) => {
        funIndices[fn.name] = i;
        funInfos[fn.name] = fn;
    });


    function buildVarIndexMap(fn: FunnyFunction): {
        vars: VarIndexMap;
        paramTypes: any[];   // типы параметров для func_type_m
        localTypes: any[];   // типы локальных переменных (returns + locals)
    } {
        const vars: VarIndexMap = {};
        const paramTypes: any[] = [];
        const localTypes: any[] = [];
        let idx = 0;

        // Параметры: индексы 0..params-1
        for (const p of fn.parameters) {
            const isArray = p.typeName === "int[]";
            vars[p.name] = { index: idx++, isArray };
            paramTypes.push(isArray ? i64 : i32);
        }

        // Возвращаемые значения и локальные переменные 
        for (const r of fn.returns) {
            const isArray = r.typeName === "int[]";
            vars[r.name] = { index: idx++, isArray };
            localTypes.push(isArray ? i64 : i32);
        }

        for (const l of fn.locals) {
            const isArray = l.typeName === "int[]";
            vars[l.name] = { index: idx++, isArray };
            localTypes.push(isArray ? i64 : i32);
        }

        return { vars, paramTypes, localTypes };
    }


    function compileArrayExpr(e: Expr, ctx: FunCompileContext): Op<any> {
        if (e && e.type === "funccall") {
            const fnInfo = ctx.funInfos[e.name];
            if (!fnInfo) {
                throw new Error(`Unknown function '${e.name}' during codegen (array expression)`);
            }

            if (fnInfo.returns.length !== 1 || fnInfo.returns[0].typeName !== "int[]") {
                throw new Error(`Function '${e.name}' cannot be used as array-valued expression`);
            }

            const fi = ctx.funIndices[e.name];
            if (fi === undefined) {
                throw new Error(`Unknown function index for '${e.name}' during codegen`);
            }

            const argsOps: Op<any>[] = [];
            for (let i = 0; i < e.args.length; i++) {
                const paramType = fnInfo.parameters[i]?.typeName ?? "int";
                const argExpr = e.args[i];
                if (paramType === "int") {
                    argsOps.push(compileExpr(argExpr, ctx)); // скаляр
                } else {
                    argsOps.push(compileArrayExpr(argExpr, ctx)); // массив
                }
            }

            return call(i64 as any, varuint32(fi) as any, argsOps);
        }

        const ae = e as { kind?: string; name?: string; [k: string]: any };
        if (ae.kind === "Ident") {
            const info = ctx.vars[ae.name!];
            if (!info || !info.isArray) {
                throw new Error(`Variable '${ae.name}' is not an array`);
            }
            return get_local(i64 as any, info.index);
        }

        throw new Error("Expected array-valued expression");
    }

    function compileExpr(e: Expr, ctx: FunCompileContext): Op<I32> {
     
        if (e && e.type === "funccall") {
            const fnInfo = ctx.funInfos[e.name];
            if (!fnInfo) {
                throw new Error(`Unknown function '${e.name}' during codegen`);
            }

            if (fnInfo.returns.length !== 1 || fnInfo.returns[0].typeName !== "int") {
                throw new Error(`Function '${e.name}' cannot be used as int-valued expression`);
            }

            const fi = ctx.funIndices[e.name];
            if (fi === undefined) {
                throw new Error(`Unknown function index for '${e.name}' during codegen`);
            }

            const argsOps: Op<any>[] = [];
            for (let i = 0; i < e.args.length; i++) {
                const paramType = fnInfo.parameters[i]?.typeName ?? "int";
                const argExpr = e.args[i];
                if (paramType === "int") {
                    argsOps.push(compileExpr(argExpr, ctx));
                } else {
                    argsOps.push(compileArrayExpr(argExpr, ctx));
                }
            }

            return call(i32, varuint32(fi) as any, argsOps);
        }

        if (e && e.type === "arraccess") {
            // a[i] -> array_get(a, i)
            const info = ctx.vars[e.name];
            if (!info || !info.isArray) {
                throw new Error(`Variable '${e.name}' is not an array (arraccess)`);
            }
            const arrOp = get_local(i64 as any, info.index);
            const idxOp = compileExpr(e.index, ctx); 
            check_bounds(arrOp, idxOp);
            return array_get(arrOp, idxOp);
        }


        const ae = e as { kind: string; [k: string]: any };

        switch (ae.kind) {
            case "Int":
                return i32.const(ae.value as number);

            case "Ident": {
                const info = ctx.vars[ae.name];
                if (!info) {
                    throw new Error(`Unknown variable '${ae.name}' during codegen`);
                }
                if (info.isArray) {
                    throw new Error(`Array variable '${ae.name}' cannot be used as int expression`);
                }
                return get_local(i32, info.index);
            }

            case "Neg": {
                const inner = compileExpr(ae.expr as Expr, ctx);
                return i32.sub(i32.const(0), inner);
            }

            case "Add": {
                const l = compileExpr(ae.left as Expr, ctx);
                const r = compileExpr(ae.right as Expr, ctx);
                return i32.add(l, r);
            }

            case "Sub": {
                const l = compileExpr(ae.left as Expr, ctx);
                const r = compileExpr(ae.right as Expr, ctx);
                return i32.sub(l, r);
            }

            case "Mul": {
                const l = compileExpr(ae.left as Expr, ctx);
                const r = compileExpr(ae.right as Expr, ctx);
                return i32.mul(l, r);
            }

            case "Div": {
                const l = compileExpr(ae.left as Expr, ctx);
                const r = compileExpr(ae.right as Expr, ctx);
                return i32.div_s(l, r);
            }

            default:
                throw new Error(`Unknown expression kind '${ae.kind}' in codegen`);
        }
    }

    function compileCondition(cond: Condition, ctx: FunCompileContext): Op<I32> {
        switch (cond.kind) {
            case "true":
                return i32.const(1);
            case "false":
                return i32.const(0);
            case "comparison": {
                const l = compileExpr(cond.left, ctx);
                const r = compileExpr(cond.right, ctx);
                switch (cond.op) {
                    case "==": return i32.eq(l, r);
                    case "!=": return i32.ne(l, r);
                    case "<":  return i32.lt_s(l, r);
                    case ">":  return i32.gt_s(l, r);
                    case "<=": return i32.le_s(l, r);
                    case ">=": return i32.ge_s(l, r);
                    default:
                        throw new Error(`Unknown comparison operator '${cond.op}'`);
                }
            }
            case "not": {
                const cnd = compileCondition(cond.condition, ctx);
                return i32.eqz(cnd);
            }
            case "and": {
                const l = compileCondition(cond.left, ctx);
                const r = compileCondition(cond.right, ctx);
                return i32.and(l, r);
            }
            case "or": {
                const l = compileCondition(cond.left, ctx);
                const r = compileCondition(cond.right, ctx);
                return i32.or(l, r);
            }
            case "paren":
                return compileCondition(cond.inner, ctx);

            default:
                throw new Error(`Unknown condition kind '${cond.kind}' in codegen`);
        }
    }

    function compileStmt(stmt: Stmt, ctx: FunCompileContext): Op<any>[] {
        switch (stmt.type) {
            case "assign": {
                const ops: Op<any>[] = [];

           
                if (stmt.targets.length !== stmt.exprs.length) {
                    throw new Error("Tuple assignment with mismatched arity is not supported in this compiler");
                }

                for (let i = 0; i < stmt.targets.length; i++) {
                    const lv = stmt.targets[i];
                    const rhsExpr = stmt.exprs[i];

                    if (lv.type === "lvar") {
                        const info = ctx.vars[lv.name];
                        if (!info) {
                            throw new Error(`Unknown variable '${lv.name}' in assignment`);
                        }

                        if (info.isArray) {
                        
                            const exprOp = compileArrayExpr(rhsExpr, ctx);
                            ops.push(set_local(info.index, exprOp as any));
                        } else {
                            const exprOp = compileExpr(rhsExpr, ctx);
                            ops.push(set_local(info.index, exprOp));
                        }
                    } else if (lv.type === "larr") {
                        // a[i] = e;
                        const info = ctx.vars[lv.name];
                        if (!info || !info.isArray) {
                            throw new Error(`'${lv.name}' is not an array in array element assignment`);
                        }
                        const arrOp = get_local(i64 as any, info.index);
                        const idxOp = compileExpr(lv.index, ctx);
                        const valOp = compileExpr(rhsExpr, ctx);

                        check_bounds(arrOp, idxOp);
                        const setOp = array_set(arrOp, idxOp, valOp);
                        // array_set возвращает значение; чтобы не оставлять его на стеке — drop
                        ops.push(c.drop(c.void, setOp));
                    } else {
                        throw new Error(`Unknown LValue type '${lv.type}'`);
                    }
                }
                return ops;
            }

            case "block": {
                const res: Op<any>[] = [];
                for (const s of stmt.stmts) {
                    res.push(...compileStmt(s, ctx));
                }
                return res;
            }

            case "expr": {
                const eop = compileExpr(stmt.expr, ctx);
                return [c.drop(c.void, eop)];
            }

            case "if": {
                const condOp = compileCondition(stmt.condition, ctx);
                const thenOps = compileStmt(stmt.then, ctx);
                const elseOps = stmt.else ? compileStmt(stmt.else, ctx) : undefined;
                return [if_(c.void, condOp, thenOps, elseOps)];
            }

            case "while": {
                const loopBody: Op<any>[] = [];

                const condOp = compileCondition(stmt.condition, ctx);
                const notCond = i32.eqz(condOp);
                loopBody.push(br_if(1, notCond));
                loopBody.push(...compileStmt(stmt.body, ctx));
                loopBody.push(c.br(0));

                return [void_block([void_loop(loopBody)])];
            }

            default:
                throw new Error(`Unknown statement type '${stmt.type}' in codegen`);
        }
    }

    const funcTypes: any[] = [];
    const funcTypeIndices: any[] = [];
    const funcBodies: any[] = [];
    const exportEntries: any[] = [];

    funs.forEach((fn, funIndex) => {
        const { vars, paramTypes, localTypes } = buildVarIndexMap(fn);
        const ctx: FunCompileContext = { funIndices, funInfos, vars };

        // Типы результатов функции (int / int[])
        const resultTypes = fn.returns.map(r =>
            r.typeName === "int" ? i32 : i64
        );

        const fType = func_type_m(paramTypes, resultTypes);
        funcTypes.push(fType);

        const typeIdx = varuint32(funcTypes.length - 1) as any;
        funcTypeIndices.push(typeIdx);

        // Локальные переменные (returns + locals): каждый по отдельности,
        // чтобы индексы совпадали с VarIndexMap
        const locals: LocalEntry[] = [];
        for (const t of localTypes) {
            locals.push(local_entry(varuint32(1) as any, t));
        }

        // Тело функции
        const bodyOps: Op<any>[] = [];
        bodyOps.push(...compileStmt(fn.body, ctx));

        // В конце кладём возвращаемые значения на стек в порядке объявлений
        for (const r of fn.returns) {
            const info = vars[r.name];
            if (!info) {
                throw new Error(`Return variable '${r.name}' not found in var map`);
            }
            const ty = r.typeName === "int" ? i32 : i64;
            bodyOps.push(get_local(ty as any, info.index));
        }

        const body = function_body(locals, bodyOps);
        funcBodies.push(body);

        // Экспортируем функцию по имени
        exportEntries.push(
            export_entry(
                str_ascii(fn.name),
                c.external_kind.function,
                varuint32(funIndex) as any,
            ),
        );
    });

    // Секции: type, function, export, code
    const typeSec = type_section(funcTypes);
    const funcSec = function_section(funcTypeIndices as any);
    const exportSec = export_section(exportEntries);
    const codeSec = code_section(funcBodies);

    const wasmModule = c.module([typeSec, funcSec, exportSec, codeSec]);

    // Эмитим в буфер
    const emitter = new BufferedEmitter(new ArrayBuffer(1 << 16));
    wasmModule.emit(emitter);
    const bytes = new Uint8Array(emitter.buffer, 0, emitter.length);

    if (name) {
        try {
            writeFileSync(name.endsWith(".wasm") ? name : `${name}.wasm`, bytes);
        } catch {
        }
    }

    const { instance } = await WebAssembly.instantiate(bytes, {});
    return instance.exports;
}

export { FunnyError } from '../../lab08'
