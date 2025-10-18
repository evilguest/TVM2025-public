export type Expr =
  | { kind: "Int"; value: number }
  | { kind: "Ident"; name: string }
  | { kind: "Neg"; expr: Expr }         
  | { kind: "Add"; left: Expr; right: Expr }
  | { kind: "Sub"; left: Expr; right: Expr }
  | { kind: "Mul"; left: Expr; right: Expr }
  | { kind: "Div"; left: Expr; right: Expr };
