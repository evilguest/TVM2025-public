import { writeFileSync } from "fs";
import { Op, I32, Void, c, BufferedEmitter, LocalEntry} from "../../wasm";
import { Module } from "../../lab08";

const { i32, 
    varuint32,
    get_local, local_entry, set_local, call, if_, void_block, void_loop, br_if, str_ascii, export_entry,
    func_type_m, function_body, type_section, function_section, export_section, code_section } = c;
  
export async function compileModule<M extends Module>(m: M, name?: string): Promise<WebAssembly.Exports>
{
    throw "Not implemented";
}
export { FunnyError } from '../../lab08'