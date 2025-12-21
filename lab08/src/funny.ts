import * as arith from "../../lab04";

export interface SourceLoc {
    file?: string;
    startLine: number;
    startCol: number;
    endLine?: number;
    endCol?: number;
}

export const enum ErrorCode {
    ParseError = 'E_PARSE_ERROR',
    DuplicateFunction = 'E_DUPLICATE_FUNCTION',
    Redeclaration = 'E_REDECLARATION',
    AssignArity = 'E_ASSIGN_ARITY',
    AssignUndeclaredVar = 'E_ASSIGN_UNDECLARED_VAR',
    AssignUndeclaredArray = 'E_ASSIGN_UNDECLARED_ARRAY',
    UseUndeclaredVar = 'E_USE_UNDECLARED_VAR',
    OperatorMultiValue = 'E_OPERATOR_MULTI_VALUE',
    UnknownFunction = 'E_UNKNOWN_FUNCTION',
    ArgumentCount = 'E_ARGUMENT_COUNT',
    ArgumentMultiValue = 'E_ARGUMENT_MULTI_VALUE',
    AccessUndeclaredArray = 'E_ACCESS_UNDECLARED_ARRAY',
    ArrayIndexMultiValue = 'E_ARRAY_INDEX_MULTI_VALUE',
    ComparisonMultiValue = 'E_COMPARISON_MULTI_VALUE',
}

export class FunnyError extends Error {
    constructor(
        message: string,
        public readonly code: string,
        public readonly startLine?: number,
        public readonly startCol?: number,
        public readonly endCol?: number,
        public readonly endLine?: number
    ) {
        super(message);
    }
}

export interface Module {
    type: "module";
    functions: FunctionDef[];
    loc?: SourceLoc;
}

export interface FunctionDef {
    type: "fun";
    name: string;
    parameters: ParameterDef[];
    returns: ParameterDef[];
    locals: ParameterDef[];
    body: Statement;
    loc?: SourceLoc;
}

export interface ParameterDef {
    type: "param";
    name: string;
    typeName: "int" | "int[]";
}

export type Statement =
    | AssignStmt
    | BlockStmt
    | IfStmt
    | WhileStmt
    | ExprStmt;

export type LValue = VarLValue | ArrLValue;

export interface VarLValue {
    type: "lvar";
    name: string;
    loc?: SourceLoc;
}

export interface ArrLValue {
    type: "larr";
    name: string;
    index: Expr;
    loc?: SourceLoc;
}

export interface AssignStmt {
    type: "assign";
    targets: LValue[];
    exprs: Expr[];
    loc?: SourceLoc;
}

export interface BlockStmt {
    type: "block";
    stmts: Statement[];
    loc?: SourceLoc;
}

export interface IfStmt {
    type: "if";
    condition: Condition;
    then: Statement;
    else: Statement | null;
    loc?: SourceLoc;
}

export interface WhileStmt {
    type: "while";
    condition: Condition;
    body: Statement;
    loc?: SourceLoc;
}

export interface ExprStmt {
    type: "expr";
    expr: Expr;
    loc?: SourceLoc;
}

export type Expr =
    | arith.Expr
    | FuncCallExpr
    | ArrAccessExpr;

export interface FuncCallExpr {
    type: "funccall";
    name: string;
    args: Expr[];
    loc?: SourceLoc;
}

export interface ArrAccessExpr {
    type: "arraccess";
    name: string;
    index: Expr;
    loc?: SourceLoc;
}

export type Condition =
    | TrueCond
    | FalseCond
    | ComparisonCond
    | NotCond
    | AndCond
    | OrCond
    | ParenCond;

export interface TrueCond {
    kind: "true";
    loc?: SourceLoc;
}

export interface FalseCond {
    kind: "false";
    loc?: SourceLoc;
}

export interface ComparisonCond {
    kind: "comparison";
    left: Expr;
    op: "==" | "!=" | ">" | "<" | ">=" | "<=";
    right: Expr;
    loc?: SourceLoc;
}

export interface NotCond {
    kind: "not";
    condition: Condition;
    loc?: SourceLoc;
}

export interface AndCond {
    kind: "and";
    left: Condition;
    right: Condition;
    loc?: SourceLoc;
}

export interface OrCond {
    kind: "or";
    left: Condition;
    right: Condition;
    loc?: SourceLoc;
}

export interface ParenCond {
    kind: "paren";
    inner: Condition;
    loc?: SourceLoc;
}
