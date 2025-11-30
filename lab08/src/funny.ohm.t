Funny <: Arithmetic {
  Expr = Exp

  Module
    = Function+

  Function
    = Ident "(" ParamList ")" RetSpec UsesSpec? Stmt

  RetSpec
    = "returns" ParamListNonEmpty   -- multi
    | "returns" Ident ":" Type      -- single

  UsesSpec
    = "uses" ParamList

  ParamList
    = ListOf<Param, ",">

  ParamListNonEmpty
    = NonemptyListOf<Param, ",">

  Param
    = Ident ":" Type

  Type
    = "int" "[]"     -- array
    | "int"          -- scalar

  ArgList
    = ListOf<Expr, ",">

  Block
    = "{" Stmt* "}"

  Stmt
    = Assign
    | Block
    | While
    | If
    | Expr ";"       -- expressionStatement

  While
    = "while" "(" Condition ")" Stmt

  If
    = "if" "(" Condition ")" Stmt ("else" Stmt)?

  Assign
    = LValueList "=" ExprList ";"   -- tupleAssign
    | LValue "=" Expr ";"           -- simpleAssign

  LValueList
    = ListOf<LValue, ",">

  ExprList
    = ListOf<Expr, ",">

  LValue
    = ArrayAccess
    | Ident

  Primary
    += FunctionCall
     | ArrayAccess

  FunctionCall
    = Ident "(" ArgList ")"

  ArrayAccess
    = Ident "[" Expr "]"

  Condition
    = OrCond

  OrCond
    = AndCond ("or" AndCond)*

  AndCond
    = NotCond ("and" NotCond)*

  NotCond
    = ("not")* AtomCond

  AtomCond
    = "true"        -- true
    | "false"       -- false
    | Comparison    -- cmp
    | ParenCond     -- paren

  ParenCond
    = "(" Condition ")"

  Comparison
    = Expr "==" Expr
    | Expr "!=" Expr
    | Expr "<=" Expr
    | Expr ">=" Expr
    | Expr "<"  Expr
    | Expr ">"  Expr

  // ---------- комментарии (лексические!) ----------

  lineComment  = "//" (~"\n" any)* ("\n" | end)
  blockComment = "/*" (~"*/" any)* "*/"

  space += lineComment | blockComment
}
