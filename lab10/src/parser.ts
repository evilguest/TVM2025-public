// lab10/src/parser.ts

import { MatchResult, Semantics } from 'ohm-js';

import grammar, { FunnierActionDict } from './funnier.ohm-bundle';

import {
  AnnotatedModule,
  AnnotatedFunction,
  AnnotatedWhileStmt,
  FormulaDef,
  Predicate,
  TruePredicate,
  FalsePredicate,
  ComparisonPredicate,
  NotPredicate,
  AndPredicate,
  OrPredicate,
  ParenPredicate,
  QuantifierPredicate,
  FormulaRefPredicate,
} from './funnier';

import {
  ParameterDef,
  Expr,
  Statement,
  Condition,
  ErrorCode,
  FunnyError,
  getFunnyAst,
} from '../../lab08';

// ----------------- утилиты -----------------

function collectList<T>(node: any): T[] {
  return node.asIteration().children.map((c: any) => c.parse() as T);
}

function foldLogicalChain<T>(
  first: any,
  rest: any,
  makeNode: (left: T, right: T) => T
): T {
  let node = first.parse() as T;
  const restChildren =
    rest.children ?? rest.asIteration?.().children ?? [];
  for (const r of restChildren) {
    const rhs = r.parse() as T;
    node = makeNode(node, rhs);
  }
  return node;
}

function repeatPrefix<T>(
  nots: any,
  base: any,
  makeNode: (inner: T) => T
): T {
  let node = base.parse() as T;
  const count =
    nots.children?.length ??
    nots.asIteration?.().children.length ??
    0;
  for (let i = 0; i < count; i++) {
    node = makeNode(node);
  }
  return node;
}

// ----------------- семантика -----------------

const getFunnierAst = {
  // всё, что не связано с верификацией, переиспользуем из lab08
  ...(getFunnyAst as any),

  // ---------- модуль: функции + формулы ----------

  // Module := Item+
  Module(items: any) {
  const functions: AnnotatedFunction[] = [];
  const formulas: FormulaDef[] = [];

  // items.children содержит последовательность Function и Formula узлов
  for (const it of items.children) {
    const node = it.parse();
    
    // Проверяем тип узла по наличию определенных свойств
    if (node.type === 'formula') {
      // Это FormulaDef
      formulas.push(node as FormulaDef);
    } else {
      // Это AnnotatedFunction
      functions.push(node as AnnotatedFunction);
    }
  }

  const mod: AnnotatedModule = {
    type: 'module',
    functions,
    formulas,
  };

  return mod;
},

  // ---------- возвращаемые значения / void ----------

  // RetOrVoid = RetSpec -- retSpec | "returns" "void" -- void
  RetOrVoid_retSpec(rs: any) {
    return rs.parse() as ParameterDef[];
  },

  RetOrVoid_void(_returnsTok: any, _voidTok: any) {
    // void-функция: список возвращаемых пустой
    return [] as ParameterDef[];
  },

  // ---------- функция с requires / ensures ----------

  // Function :=
  //   Ident "(" ParamList ")"
  //     RequiresSpec?
  //     RetOrVoid
  //     EnsuresSpec?
  //     UsesSpec?
  //     Stmt
  Function(
    name: any,
    _lp: any,
    paramsNode: any,
    _rp: any,
    requiresOpt: any,
    retOrVoid: any,
    ensuresOpt: any,
    usesOpt: any,
    stmt: any
  ) {
    const nameStr = name.sourceString;
    const parameters = paramsNode.parse() as ParameterDef[];
    const returns = retOrVoid.parse() as ParameterDef[];   // [] если void
    const locals =
      usesOpt.children.length > 0
        ? (usesOpt.child(0).parse() as ParameterDef[])
        : [];

    const requires =
      requiresOpt.children.length > 0
        ? (requiresOpt.child(0).parse() as Predicate)
        : undefined;

    const ensures =
      ensuresOpt.children.length > 0
        ? (ensuresOpt.child(0).parse() as Predicate)
        : undefined;

    const body = stmt.parse() as Statement;

    const fn: AnnotatedFunction = {
      type: 'fun',
      name: nameStr,
      parameters,
      returns,
      locals,
      body,
      requires,
      ensures,
    };

    return fn;
  },

  RequiresSpec(_requires: any, pred: any) {
    return pred.parse() as Predicate;
  },

  EnsuresSpec(_ensures: any, pred: any) {
    return pred.parse() as Predicate;
  },

  // ---------- while с инвариантом ----------

  // While := "while" "(" Condition ")" InvariantSpec? Stmt
  While(_while: any, _lp: any, cond: any, _rp: any, invOpt: any, body: any) {
    const condition = cond.parse() as Condition;
    const invariant =
      invOpt.children.length > 0
        ? (invOpt.child(0).parse() as Predicate)
        : undefined;
    const bodyStmt = body.parse() as Statement;

    const ws: AnnotatedWhileStmt = {
      type: 'while',
      condition,
      body: bodyStmt,
      invariant,
    };

    return ws;
  },

  InvariantSpec(_inv: any, pred: any) {
    return pred.parse() as Predicate;
  },

  // ---------- формулы ----------

  // Formula = "formula" Ident "(" ParamList ")" "=>" Predicate ";"
  Formula(
    name: any,
    _lp: any,
    paramsNode: any,
    _rp: any,
    _arrow: any,
    bodyPred: any,
    _semi: any
  ) {
    const nameStr = name.sourceString;
    const params = paramsNode.parse() as ParameterDef[];
    const body = bodyPred.parse() as Predicate;

    const f: FormulaDef = {
      type: 'formula',
      name: nameStr,
      parameters: params,
      body,
    };
    return f;
  },

  // ---------- предикаты ----------

  // Predicate = OrPred
  Predicate(orNode: any) {
    return orNode.parse() as Predicate;
  },

  OrPred(first: any, _ops: any, rest: any) {
    return foldLogicalChain<Predicate>(
      first,
      rest,
      (left, right) =>
        ({
          kind: 'or',
          left,
          right,
        } as OrPredicate)
    );
  },

  AndPred(first: any, _ops: any, rest: any) {
    return foldLogicalChain<Predicate>(
      first,
      rest,
      (left, right) =>
        ({
          kind: 'and',
          left,
          right,
        } as AndPredicate)
    );
  },

  NotPred(nots: any, atom: any) {
    return repeatPrefix<Predicate>(
      nots,
      atom,
      (inner) =>
        ({
          kind: 'not',
          inner,
        } as NotPredicate)
    );
  },

  AtomPred_true(_t: any) {
    const n: TruePredicate = { kind: 'true' };
    return n;
  },

  AtomPred_false(_f: any) {
    const n: FalsePredicate = { kind: 'false' };
    return n;
  },

  // Comparison из lab08 возвращает узел с полями left/op/right
  AtomPred_cmp(comp: any) {
    const c = comp.parse() as any;
    const n: ComparisonPredicate = {
      kind: 'comparison',
      left: c.left as Expr,
      op: c.op,
      right: c.right as Expr,
    };
    return n;
  },

  AtomPred_quant(q: any) {
    return q.parse() as QuantifierPredicate;
  },

  AtomPred_formulaRef(fr: any) {
    return fr.parse() as FormulaRefPredicate;
  },

  AtomPred_paren(p: any) {
    return p.parse() as ParenPredicate;
  },

  ParenPred(_lp: any, inner: any, _rp: any) {
    const p: ParenPredicate = {
      kind: 'paren',
      inner: inner.parse() as Predicate,
    };
    return p;
  },

  // Quantifier = ("forall" | "exists") "(" Param "|" Predicate ")"
  Quantifier(
    qTok: any,
    _lp: any,
    paramNode: any,
    _bar: any,
    pred: any,
    _rp: any
  ) {
    const quantifier = qTok.sourceString as 'forall' | 'exists';
    const variable = paramNode.parse() as ParameterDef;
    const body = pred.parse() as Predicate;

    const q: QuantifierPredicate = {
      kind: 'quantifier',
      quantifier,
      variable,
      predicate: body,
    };

    return q;
  },

  // FormulaRef = Ident "(" ArgList ")"
  FormulaRef(name: any, _lp: any, argsNode: any, _rp: any) {
    const fr: FormulaRefPredicate = {
      kind: 'formulaRef',
      name: name.sourceString,
      args: argsNode.parse() as Expr[],
    };
    return fr;
  },

  // переопределяем ParamList, чтобы использовать наш collectList
  ParamList(list: any) {
    return collectList<ParameterDef>(list);
  },
} satisfies FunnierActionDict<any>;

// ----------------- Semantics / parseFunnier -----------------

export const semantics: FunnySemanticsExt =
  grammar.Funnier.createSemantics() as FunnySemanticsExt;

semantics.addOperation('parse()', getFunnierAst);

export interface FunnySemanticsExt extends Semantics {
  (match: MatchResult): FunnyActionsExt;
}

interface FunnyActionsExt {
  parse(): AnnotatedModule;
}

export function parseFunnier(source: string, origin?: string): AnnotatedModule {
  const match: MatchResult = grammar.Funnier.match(source, 'Module');

  if (match.failed()) {
    const m: any = match;
    const pos =
      typeof m.getRightmostFailurePosition === 'function'
        ? m.getRightmostFailurePosition()
        : null;

    const message: string =
      m.message ?? 'Syntax error in Funnier module.';

    throw new FunnyError(
      message,
      ErrorCode.ParseError,
      pos?.lineNum,
      pos?.colNum
    );
  }

  const mod = (semantics as FunnySemanticsExt)(match).parse();
  return mod;
}
