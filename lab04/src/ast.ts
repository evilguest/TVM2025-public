export type Expr =
  | { kind: "Int"; value: number }
  | { kind: "Ident"; name: string }
  | { kind: "Neg"; expr: Expr }
  | Binary;

type Binary = {
  readonly kind: "Add" | "Sub" | "Mul" | "Div"; 
  left: Expr; right: Expr 
}

export interface Add extends Binary {
  readonly kind: "Add"; 
}