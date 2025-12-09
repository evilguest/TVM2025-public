Funnier <: Funny {

  // ---------- Модуль: функции + формулы ----------

  Module :=
    (Function | Formula)+

  // ---------- Функция с аннотациями ----------

  Function :=
    Ident "(" ParamList ")"
      RequiresSpec?
      RetOrVoid
      EnsuresSpec?
      UsesSpec?
      Stmt

  RetOrVoid
    = RetSpec           -- retSpec
    | "returns" "void"  -- void

  RequiresSpec =
    "requires" Predicate

  EnsuresSpec =
    "ensures" Predicate

  // ---------- while с инвариантом ----------

  While :=
    "while" "(" Condition ")" InvariantSpec? Stmt

  InvariantSpec =
    "invariant" Predicate

  // ---------- определения формул ----------

  // как в описании языка: f(x: int, a: int[]) => Predicate;
  Formula =
    Ident "(" ParamList ")" "=>" Predicate ";"

  // ---------- предикаты, кванторы, ссылки на формулы ----------

  // not > and > (or, ->)
  Predicate =
    OrPred

  OrPred =
    AndPred (("or" | "->") AndPred)*

  AndPred =
    NotPred ("and" NotPred)*

  NotPred =
    ("not")* AtomPred

  AtomPred =
      "true"          -- true
    | "false"         -- false
    | Comparison      -- cmp  
    | Quantifier      -- quant
    | FormulaRef      -- formulaRef
    | ParenPred       -- paren

  ParenPred =
    "(" Predicate ")"

  Quantifier =
    ("forall" | "exists")
      "(" Param "|" Predicate ")"

  FormulaRef =
    Ident "(" ArgList ")"
}
