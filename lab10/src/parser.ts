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
  SourceLoc,
  getFunnyAst,
} from '../../lab08';

// ----------------- loc helpers -----------------

let currentOrigin: string | undefined;

function intervalToLoc(interval: any): SourceLoc {
  // interval может быть Interval или Node.
  // Нормальный путь: interval.source (Interval) -> Source с getLineAndColumn
  const src =
    interval?.source?.getLineAndColumn
      ? interval.source
      : interval?._node?.source?.getLineAndColumn
        ? interval._node.source
        : interval?.getLineAndColumn
          ? interval
          : undefined;

  const startIdx = interval?.startIdx ?? interval?._node?.source?.startIdx ?? 0;
  const endIdx = interval?.endIdx ?? interval?._node?.source?.endIdx ?? startIdx;

  const start =
    typeof src?.getLineAndColumn === "function" ? src.getLineAndColumn(startIdx) : { lineNum: 1, colNum: 1 };

  const end =
    typeof src?.getLineAndColumn === "function" ? src.getLineAndColumn(endIdx) : { lineNum: start.lineNum, colNum: start.colNum };

  return {
    file: currentOrigin,
    startLine: start.lineNum,
    startCol: start.colNum,
    endLine: end.lineNum,
    endCol: end.colNum,
  };
}


function attachLoc<T extends object>(node: T, loc?: SourceLoc): T {
  if (!node || typeof node !== "object") return node;
  if (!loc) return node;
  const anyN: any = node;
  // НЕ перетираем существующую локацию
  if (!anyN.loc) anyN.loc = loc;
  return node;
}


// ----------------- утилиты -----------------

function collectList<T>(node: any): T[] {
  return node.asIteration().children.map((c: any) => c.parse() as T);
}

function repeatPrefix<T>(
  nots: any,
  base: any,
  makeNode: (inner: T, loc: SourceLoc) => T,
  locForAll: SourceLoc
): T {
  let node = base.parse() as T;
  const count = nots.children?.length ?? nots.asIteration?.().children.length ?? 0;
  for (let i = 0; i < count; i++) {
    node = makeNode(node, locForAll);
  }
  return node;
}

// ----------------- семантика -----------------

const getFunnierAst = {
  ...(getFunnyAst as any),

  // ---------- модуль: функции + формулы ----------

  Module(items: any) {
    const loc = intervalToLoc(this.source);

    const functions: AnnotatedFunction[] = [];
    const formulas: FormulaDef[] = [];

    for (const it of items.children) {
      const node = it.parse();
      if (node?.type === 'formula') formulas.push(node as FormulaDef);
      else functions.push(node as AnnotatedFunction);
    }

    return {
      type: 'module',
      functions,
      formulas,
      loc,
    } as AnnotatedModule;
  },

  // ---------- возвращаемые значения / void ----------

  RetOrVoid_retSpec(rs: any) {
    return rs.parse() as ParameterDef[];
  },

  RetOrVoid_void(_returnsTok: any, _voidTok: any) {
    return [] as ParameterDef[];
  },

  // ---------- функция с requires / ensures ----------

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
    const loc = intervalToLoc(this.source);

    const nameStr = name.sourceString;
    const parameters = paramsNode.parse() as ParameterDef[];
    const returns = retOrVoid.parse() as ParameterDef[];
    const locals =
      usesOpt.children.length > 0
        ? (usesOpt.child(0).parse() as ParameterDef[])
        : [];

    const requires =
  requiresOpt.children.length > 0
    ? (requiresOpt.child(0).parse() as Predicate) // loc уже стоит в RequiresSpec
    : undefined;

const ensures =
  ensuresOpt.children.length > 0
    ? (ensuresOpt.child(0).parse() as Predicate) // loc уже стоит в EnsuresSpec
    : undefined;

    const body = stmt.parse() as Statement;

    return {
      type: 'fun',
      name: nameStr,
      parameters,
      returns,
      locals,
      body,
      requires,
      ensures,
      loc,
    } as AnnotatedFunction;
  },

  RequiresSpec(_requires: any, pred: any) {
    const loc = intervalToLoc(this.source);
    return attachLoc(pred.parse() as Predicate, loc);
  },

  EnsuresSpec(_ensures: any, pred: any) {
    const loc = intervalToLoc(this.source);
    return attachLoc(pred.parse() as Predicate, loc);
  },

  // ---------- while с инвариантом ----------

  While(_while: any, _lp: any, cond: any, _rp: any, invOpt: any, body: any) {
    const loc = intervalToLoc(this.source);

    const condition = cond.parse() as Condition;
    const invariant =
  invOpt.children.length > 0
    ? (invOpt.child(0).parse() as Predicate) // loc уже стоит в InvariantSpec
    : undefined;

    const bodyStmt = body.parse() as Statement;

    return {
      type: 'while',
      condition,
      body: bodyStmt,
      invariant,
      loc,
    } as AnnotatedWhileStmt;
  },

  InvariantSpec(_inv: any, pred: any) {
    const loc = intervalToLoc(this.source);
    return attachLoc(pred.parse() as Predicate, loc);
  },

  // ---------- формулы ----------

  Formula(
    name: any,
    _lp: any,
    paramsNode: any,
    _rp: any,
    _arrow: any,
    bodyPred: any,
    _semi: any
  ) {
    const loc = intervalToLoc(this.source);

    return {
      type: 'formula',
      name: name.sourceString,
      parameters: paramsNode.parse() as ParameterDef[],
      body: bodyPred.parse() as Predicate,
      loc,
    } as FormulaDef;
  },

  // ---------- предикаты ----------

  Predicate(orNode: any) {
    return orNode.parse() as Predicate;
  },

 OrPred(first: any, _ops: any, rest: any) {
  const loc = intervalToLoc(this.source);

  let node = first.parse() as Predicate;
  const restChildren = rest.children ?? rest.asIteration?.().children ?? [];
  for (const r of restChildren) {
    const rhs = r.parse() as Predicate;
    node = {
      kind: "or",
      left: node,
      right: rhs,
      loc,
    } as any as OrPredicate;
  }

  //  ВАЖНО: даже если OR-цепочка длины 1 — приклеиваем loc к node,
  // чтобы simple predicate (comparison) тоже имел координаты.
  return attachLoc(node as any, loc);
},

AndPred(first: any, _ops: any, rest: any) {
  const loc = intervalToLoc(this.source);

  let node = first.parse() as Predicate;
  const restChildren = rest.children ?? rest.asIteration?.().children ?? [];
  for (const r of restChildren) {
    const rhs = r.parse() as Predicate;
    node = {
      kind: "and",
      left: node,
      right: rhs,
      loc,
    } as any as AndPredicate;
  }

  //  то же самое: единичный comparison теперь получает loc всего AndPred
  return attachLoc(node as any, loc);
},

NotPred(nots: any, atom: any) {
  const loc = intervalToLoc(this.source);

  const node = repeatPrefix<Predicate>(
    nots,
    atom,
    (inner) =>
      ({
        kind: "not",
        inner,
        loc,
      } as NotPredicate),
    loc
  );

  //  Если not'ов 0, repeatPrefix вернул atom.parse() без оболочки.
  // Приклеиваем loc, чтобы голый comparison не оставался без координат.
  return attachLoc(node as any, loc);
},


  AtomPred_true(_t: any) {
    const loc = intervalToLoc(this.source);
    const n: TruePredicate = { kind: 'true', loc };
    return n;
  },

  AtomPred_false(_f: any) {
    const loc = intervalToLoc(this.source);
    const n: FalsePredicate = { kind: 'false', loc };
    return n;
  },

  AtomPred_cmp(comp: any) {
    // Comparison из lab08 уже содержит left/op/right (+ loc, если ты добавил туда loc)
    // но мы всё равно можем сверху прицепить loc от текущего узла AtomPred_cmp
    const loc = intervalToLoc(this.source);
    const c = comp.parse() as any;
    const n: ComparisonPredicate = {
      kind: 'comparison',
      left: c.left as Expr,
      op: c.op,
      right: c.right as Expr,
      loc: c.loc ?? loc,
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
    const loc = intervalToLoc(this.source);
    return {
      kind: 'paren',
      inner: inner.parse() as Predicate,
      loc,
    } as ParenPredicate;
  },

  Quantifier(
    qTok: any,
    _lp: any,
    paramNode: any,
    _bar: any,
    pred: any,
    _rp: any
  ) {
    const loc = intervalToLoc(this.source);
    return {
      kind: 'quantifier',
      quantifier: qTok.sourceString as 'forall' | 'exists',
      variable: paramNode.parse() as ParameterDef,
      predicate: pred.parse() as Predicate,
      loc,
    } as QuantifierPredicate;
  },

  FormulaRef(name: any, _lp: any, argsNode: any, _rp: any) {
    const loc = intervalToLoc(this.source);
    return {
      kind: 'formulaRef',
      name: name.sourceString,
      args: argsNode.parse() as Expr[],
      loc,
    } as FormulaRefPredicate;
  },

  ParamList(list: any) {
    return collectList<ParameterDef>(list);
  },
} satisfies FunnierActionDict<any>;

// ----------------- Semantics / parseFunnier -----------------

export const semantics: FunnySemanticsExt =
  grammar.Funnier.createSemantics() as FunnySemanticsExt;

// фикс типизации ohm action dict
semantics.addOperation('parse()', getFunnierAst as any);

export interface FunnySemanticsExt extends Semantics {
  (match: MatchResult): FunnyActionsExt;
}

interface FunnyActionsExt {
  parse(): AnnotatedModule;
}

export function parseFunnier(source: string, origin?: string): AnnotatedModule {
  currentOrigin = origin;

  const match: MatchResult = grammar.Funnier.match(source, 'Module');

  if (match.failed()) {
    const m: any = match;
    const pos =
      typeof m.getRightmostFailurePosition === 'function'
        ? m.getRightmostFailurePosition()
        : null;

    const message: string = m.message ?? 'Syntax error in Funnier module.';

    currentOrigin = undefined;
    throw new FunnyError(message, ErrorCode.ParseError, pos?.lineNum, pos?.colNum);
  }

  const mod = (semantics as FunnySemanticsExt)(match).parse();

  currentOrigin = undefined;
  return mod;
}
