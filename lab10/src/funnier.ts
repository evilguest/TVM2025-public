// lab10/src/funnier.ts

import type {
  Module as FunnyModule,
  FunctionDef as FunnyFunctionDef,
  WhileStmt as FunnyWhileStmt,
  ParameterDef,
  Expr,
  ComparisonCond,
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
}

export interface FalsePredicate {
  kind: 'false';
}

export interface ComparisonPredicate {
  kind: 'comparison';
  left: Expr;
  op: ComparisonCond['op']; // "==", "!=", "<", ">", "<=", ">="
  right: Expr;
}

export interface NotPredicate {
  kind: 'not';
  inner: Predicate;
}

export interface AndPredicate {
  kind: 'and';
  left: Predicate;
  right: Predicate;
}

export interface OrPredicate {
  kind: 'or';
  left: Predicate;
  right: Predicate;
}

export interface ParenPredicate {
  kind: 'paren';
  inner: Predicate;
}

export type QuantifierKind = 'forall' | 'exists';

export interface QuantifierPredicate {
  kind: 'quantifier';
  quantifier: QuantifierKind;
  variable: ParameterDef; // имя + тип (как в параметрах функции)
  predicate: Predicate;
}

export interface FormulaRefPredicate {
  kind: 'formulaRef';
  name: string;
  args: Expr[];
}

// ---------- Формулы ----------

export interface FormulaDef {
  type: 'formula';
  name: string;
  parameters: ParameterDef[];
  body: Predicate;
}

// ---------- Аннотированные конструкции ----------

export interface AnnotatedWhileStmt extends FunnyWhileStmt {
  // while (cond) invariant <Predicate> stmt;
  invariant?: Predicate;
}

export interface AnnotatedFunction extends FunnyFunctionDef {
  // requires / ensures в аннотациях функции
  requires?: Predicate;
  ensures?: Predicate;
}

// Модуль: тот же, что в lab08, плюс:
//   - функции с аннотациями
//   - список формул
export interface AnnotatedModule extends FunnyModule {
  functions: AnnotatedFunction[];
  formulas: FormulaDef[];
}
