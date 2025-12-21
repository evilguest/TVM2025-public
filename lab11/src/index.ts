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

function printExcerpt(source: string, line: number, col: number, contextLines = 1) {
  const lines = source.split(/\r?\n/);
  const i = Math.max(0, Math.min(lines.length - 1, line - 1));

  const from = Math.max(0, i - contextLines);
  const to = Math.min(lines.length - 1, i + contextLines);

  for (let k = from; k <= to; k++) {
    const ln = k + 1;
    const prefix = ln === line ? ">" : " ";
    console.error(`${prefix} ${String(ln).padStart(4, " ")} | ${lines[k]}`);
    if (ln === line) {
      const caretPos = Math.max(0, col - 1);
      console.error(`  ${" ".repeat(4)} | ${" ".repeat(caretPos)}^`);
    }
  }
}

// -------------------- entry --------------------

export async function parseVerifyAndCompile(name: string, source: string) {
  try {
    const ast = parseFunnier(source, name);
    await verifyModule(ast);
    const mod = await compileModule(ast, name);
    return new ExportWrapper(mod);
  } catch (e: any) {
    console.error("=== parseVerifyAndCompile failed ===");
    console.error(`Sample: ${name}`);

    // Сообщение 
    if (e?.message) console.error(e.message);

    // Код ошибки (если есть)
    if (e?.code) console.error(`Code: ${e.code}`);

    // Координаты + подсветка
    if (hasLoc(e)) {
      console.error(`Location: ${formatLocFromError(e)}`);
      printExcerpt(source, e.startLine, e.startCol, 1);
    } else {
      // fallback: если loc нет, оставим stack как раньше
      console.error(e?.stack ?? e);
    }

    throw e;
  }
}
