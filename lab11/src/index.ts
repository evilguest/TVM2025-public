import { ExportWrapper, compileModule } from "../../lab09";
import { parseFunnier } from "../../lab10";
import { verifyModule } from "./verifier";

// ВАЖНО: сигнатура (name, source), как ожидает testFilesInFolderAsync
export async function parseVerifyAndCompile(name: string, source: string) {
  try {
    const ast = parseFunnier(source, name);
    await verifyModule(ast);
    const mod = await compileModule(ast, name);
    return new ExportWrapper(mod);
  } catch (e: any) {
    console.error("=== parseVerifyAndCompile failed ===");
    console.error(e?.stack ?? e);
    throw e;
  }
}
