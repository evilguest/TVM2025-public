import { c as C, Op, I32 } from "../../wasm";
import { Expr } from "../../lab04";
import { buildOneFunctionModule, Fn } from "./emitHelper";
const { i32, get_local} = C;
    
export function getVariables(e: Expr): string[] {
    const vars: string[] = [];
  const seen = new Set<string>();

  function visit(node: Expr) {
    switch (node.kind) {
      case "Ident":
        if (!seen.has(node.name)) {
          seen.add(node.name);
          vars.push(node.name);
        }
        break;

      case "Int":
        break;

      case "Neg":
        visit(node.expr);
        break;

      case "Add":
      case "Sub":
      case "Mul":
      case "Div":
        visit(node.left);
        visit(node.right);
        break;
    }
  }

  visit(e);
  return vars;
}

export async function buildFunction(e: Expr, variables: string[]): Promise<Fn<number>>
{
    let expr = wasm(e, variables)
    return await buildOneFunctionModule("test", variables.length, [expr]);
}

function wasm(e: Expr, args: string[]): Op<I32> {
    switch (e.kind) {
    case "Int":
      return i32.const(e.value);

    case "Ident": {
      const idx = args.indexOf(e.name);
      if (idx === -1)
        throw new WebAssembly.RuntimeError(`Unknown variable ${e.name}`);
      return get_local(i32, idx);
    }

    case "Neg":
      return i32.sub(i32.const(0), wasm(e.expr, args));

    case "Add":
      return i32.add(wasm(e.left, args), wasm(e.right, args));

    case "Sub":
      return i32.sub(wasm(e.left, args), wasm(e.right, args));

    case "Mul":
      return i32.mul(wasm(e.left, args), wasm(e.right, args));

    case "Div":
      // i32.div_s выбрасывает исключение при делении на 0
      return i32.div_s(wasm(e.left, args), wasm(e.right, args));
  }
}
