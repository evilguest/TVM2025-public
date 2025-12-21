// lab10/src/funnier.ts

import type {
  Module as FunnyModule,
  FunctionDef as FunnyFunctionDef,
  WhileStmt as FunnyWhileStmt,
  ParameterDef,
  Expr,
  ComparisonCond,
  SourceLoc, // <-- ВАЖНО: из lab08 теперь экспортируется SourceLoc
} from '../../lab08';

// ---------- Предикаты ----------

export type Predicate =
  | TruePredicate
  | FalsePredicate
  | ComparisonPredicate
  | NotPredicate
  | AndPredicate
  | OrPredicate
  | ParenPredicate
  | QuantifierPredicate
  | FormulaRefPredicate;

export interface TruePredicate {
  kind: 'true';
  loc?: SourceLoc;
}

export interface FalsePredicate {
  kind: 'false';
  loc?: SourceLoc;
}

export interface ComparisonPredicate {
  kind: 'comparison';
  left: Expr;
  op: ComparisonCond['op'];
  right: Expr;
  loc?: SourceLoc;
}

export interface NotPredicate {
  kind: 'not';
  inner: Predicate;
  loc?: SourceLoc;
}

export interface AndPredicate {
  kind: 'and';
  left: Predicate;
  right: Predicate;
  loc?: SourceLoc;
}

export interface OrPredicate {
  kind: 'or';
  left: Predicate;
  right: Predicate;
  loc?: SourceLoc;
}

export interface ParenPredicate {
  kind: 'paren';
  inner: Predicate;
  loc?: SourceLoc;
}

export type QuantifierKind = 'forall' | 'exists';

export interface QuantifierPredicate {
  kind: 'quantifier';
  quantifier: QuantifierKind;
  variable: ParameterDef; // имя + тип (как в параметрах функции)
  predicate: Predicate;
  loc?: SourceLoc;
}

export interface FormulaRefPredicate {
  kind: 'formulaRef';
  name: string;
  args: Expr[];
  loc?: SourceLoc;
}

// ---------- Формулы ----------

export interface FormulaDef {
  type: 'formula';
  name: string;
  parameters: ParameterDef[];
  body: Predicate;
  loc?: SourceLoc;
}

// ---------- Аннотированные конструкции ----------

export interface AnnotatedWhileStmt extends FunnyWhileStmt {
  invariant?: Predicate;
  loc?: SourceLoc;
}

export interface AnnotatedFunction extends FunnyFunctionDef {
  requires?: Predicate;
  ensures?: Predicate;
  loc?: SourceLoc;
}

export interface AnnotatedModule extends FunnyModule {
  functions: AnnotatedFunction[];
  formulas: FormulaDef[];
  loc?: SourceLoc;
}
