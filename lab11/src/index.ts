import { ExportWrapper, compileModule } from "../../lab09";
import { parseFunnier } from "../../lab10";
import { verifyModule } from "./verifier";

// -------------------- pretty printing helpers --------------------

function hasLoc(e: any): boolean {
  return typeof e?.startLine === "number" && typeof e?.startCol === "number";
}

function formatLocFromError(e: any): string {
  const sl = e?.startLine;
  const sc = e?.startCol;
  const el = e?.endLine;
  const ec = e?.endCol;

  if (typeof sl === "number" && typeof sc === "number") {
    if (typeof el === "number" && typeof ec === "number") return `${sl}:${sc}-${el}:${ec}`;
    return `${sl}:${sc}`;
  }
  return "<unknown>";
}

function buildExcerpt(source: string, line: number, col: number, contextLines = 0): string {
  const lines = source.split(/\r?\n/);
  const i = Math.max(0, Math.min(lines.length - 1, line - 1));

  const from = Math.max(0, i - contextLines);
  const to = Math.min(lines.length - 1, i + contextLines);

  const out: string[] = [];

  for (let k = from; k <= to; k++) {
    const ln = k + 1;
    const prefix = ln === line ? ">" : " ";
    out.push(`${prefix} ${String(ln).padStart(4, " ")} | ${lines[k]}`);

    if (ln === line) {
      const caretPos = Math.max(0, col - 1);
      out.push(`  ${" ".repeat(4)} | ${" ".repeat(caretPos)}^`);
    }
  }

  return out.join("\n");
}

// -------------------- entry --------------------

export async function parseVerifyAndCompile(name: string, source: string) {
  try {
    const ast = parseFunnier(source, name);
    await verifyModule(ast);
    const mod = await compileModule(ast, name);
    return new ExportWrapper(mod);
  } catch (e: any) {
    const parts: string[] = [];
    parts.push("=== parseVerifyAndCompile failed ===");
    parts.push(`Sample: ${name}`);

    if (e?.message) parts.push(String(e.message));
    if (e?.code) parts.push(`Code: ${e.code}`);

    if (hasLoc(e)) {
      parts.push(`Location: ${formatLocFromError(e)}`);
      
      parts.push(buildExcerpt(source, e.startLine, e.startCol, 0));
    } else if (e?.stack) {
      parts.push(String(e.stack));
    } else {
      parts.push(String(e));
    }

    console.error(parts.join("\n"));

    throw e;
  }
}
