import { MatchResult } from 'ohm-js';
import { arithGrammar, ArithmeticActionDict, ArithmeticSemantics, SyntaxError } from '../../lab03';
import { Expr } from './ast';

export const getExprAst: ArithmeticActionDict<Expr> = {
     Program_main(_ws1, e, _ws2, _end): Expr {
    return e.parse();
  },

  // Exp = AddExp
  Exp(addExp): Expr {
    return addExp.parse();
  },

  // AddExp = MulExp (AddOp MulExp)*
  AddExp_iter(first, ops, terms): Expr {
    let node: Expr = first.parse();

    const opNodes = ops.children;     
    const termNodes = terms.children; 

    for (let i = 0; i < opNodes.length; i++) {
      const op = opNodes[i].sourceString;
      const rhs: Expr = termNodes[i].parse();
      node = op === '+'
        ? { kind: 'Add', left: node, right: rhs }
        : { kind: 'Sub', left: node, right: rhs };
    }
    return node;
  },

  // MulExp = UnaryExp (MulOp UnaryExp)*  -- iter
  MulExp_iter(first, ops, terms): Expr {
    let node: Expr = first.parse();

    const opNodes = ops.children;
    const termNodes = terms.children;

    for (let i = 0; i < opNodes.length; i++) {
      const op = opNodes[i].sourceString;
      const rhs: Expr = termNodes[i].parse();
      node = op === '*'
        ? { kind: 'Mul', left: node, right: rhs }
        : { kind: 'Div', left: node, right: rhs };
    }
    return node;
  },

  // UnaryExp = (UnaryMinus)* Primary
  UnaryExp(minusesIter, prim): Expr {
    let node: Expr = prim.parse();
    if (minusesIter.children.length % 2 === 1) {
      node = { kind: 'Neg', expr: node };
    }
    return node;
  },

  // Primary = Int --int
  Primary_int(i): Expr {
    return i.parse();
  },

  // Primary = Ident --ident
  Primary_ident(id): Expr {
    return id.parse();
  },

  // Primary = "(" Exp ")" --paren
  Primary_paren(_l, e, _r): Expr {
    return e.parse();
  },

  Int(_digits): Expr {
    return { kind: 'Int', value: Number(this.sourceString) };
  },

  Ident(_tok): Expr {
    return { kind: 'Ident', name: this.sourceString };
  },
}

export const semantics = arithGrammar.createSemantics();
semantics.addOperation("parse()", getExprAst);

export interface ArithSemanticsExt extends ArithmeticSemantics
{
    (match: MatchResult): ArithActionsExt
}

export interface ArithActionsExt 
{
    parse(): Expr
}
export function parseExpr(source: string): Expr
{
    const m = arithGrammar.match(source);
  if (!m.succeeded()) {
    throw new SyntaxError(m.message);
  }
  const s = (semantics as ArithSemanticsExt)(m);
  return s.parse();
}


    
