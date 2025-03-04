import assert from "assert";
import { ResolvedAnnotatedFunction, ResolvedAnnotatedModule } from "@tvm/lab10";
import { match } from "ts-pattern";
import { Arith, ArithSort, Bool, Context, init, Model, SMTArray, SMTArraySort } from "z3-solver";
import { printZ3Model } from "./printZ3Model";


async function initZ3()
{
    if(!z3)
    {
        const Z3C = (await init()).Context;
        z3 = Z3C('main');        
    }
}

export let z3: Context;

export async function verifyModule(module: ResolvedAnnotatedModule)
{
    await initZ3();
    for(const f of module.functions)
    {
        const conditions = buildFunctionVerificationConditions(f);
        const z3theorem = convertConditionsToZ3(conditions);
        const res = await proveTheorem(z3theorem);
        match(res)
        .with("ok", _ => { // ok!
        })
        .otherwise(m => {
            throw new Error(`Error: function doesn't match the specification. Here is how:\n` + printZ3Model(m))
        });
    }
}

function convertConditionsToZ3(p: Predicate): Bool
{
    throw "Not implemented";
}
async function proveTheorem(b: Bool): Promise<"ok" | Model>
{
    throw "Not implemented"
} 

function buildFunctionVerificationConditions(f: ResolvedAnnotatedFunction): Predicate
{
    throw "Not implemented";
}


interface Predicate
{
    // todo: add features 
}