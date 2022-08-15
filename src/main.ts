import { match as matchVariant } from "itsamatch";
import { Decl, Prog } from "./ast/bitter";
import { Context } from './ast/context';
import { Prog as SweetProg } from "./ast/sweet";
import { createLLVMCompiler } from "./codegen/llvm/codegen";
import { Error } from './errors/errors';
import { infer } from "./infer/infer";
import { TypeContext } from "./infer/typeContext";
import { MonoTy, PolyTy, TypeParams } from "./infer/types";
import { createNodeFileSystem } from './resolve/nodefs';
import { resolve } from './resolve/resolve';
import { panic } from "./utils/misc";

enum DebugLvl {
  nothing = 0,
  sections = 1,
  types = 2,
  sweet = 3,
  all = 4,
}

let debugLevel = DebugLvl.nothing;

// pipeline:
// <string> -> parse -> <sweet> -> desugar & resolve modules -> <bitter> -> infer ->
// monomorphize -> inject reference counting -> emit code

const typeCheck = (prog: Prog): [TypeContext, Error[]] => {
  Context.clear();
  const [typingErrors, typeCtx] = infer(prog);
  return [typeCtx, typingErrors];
};

const getWasmMainFunc = async (source: string): Promise<Function> => {
  const { readFile } = await import('fs/promises');

  const wasm = await readFile(source);
  const module = new WebAssembly.Module(wasm);
  const instance = new WebAssembly.Instance(module, {});

  if ('main' in instance.exports && typeof instance.exports['main'] === 'function') {
    return instance.exports['main'];
  }

  return panic('main function not found');
};

const run = async (source: string): Promise<boolean> => {
  const logErrors = (errors: Error[]) => {
    errors.forEach(err => {
      console.log('\x1b[31m%s\x1b[0m', Error.show(err));
    });
  };

  const nfs = await createNodeFileSystem();
  const [sweetProg, errs1] = await resolve(source, nfs);

  if (errs1.length > 0) {
    logErrors(errs1);
    return false;
  }

  if (debugLevel >= DebugLvl.sweet) {
    console.log('--- sweet ---');
    console.log(SweetProg.show(sweetProg) + '\n');
  }

  const [prog, errs2] = Prog.fromSweet(sweetProg);

  if (errs2.length > 0) {
    logErrors(errs2);
    return false;
  }

  const [_, errs3] = typeCheck(prog);

  if (errs3.length > 0) {
    logErrors(errs3);
    return false;
  }

  if (debugLevel >= DebugLvl.types) {
    console.log('--- types ---');
    console.log(showTypes(prog).join('\n\n') + '\n');
  }

  const compiler = await createLLVMCompiler();
  const module = compiler.compile(prog);
  module.setSourceFileName(source);

  if (debugLevel >= DebugLvl.sections) {
    console.log(module.print());
  }

  const outPath = 'out';
  await compiler.compileIR(module, outPath, 3);
  const mainFunc = await getWasmMainFunc(`${outPath}.wasm`);

  console.log(mainFunc());

  return true;
};

const showTypes = (decls: Decl[]): string[] => {
  const types: string[] = [];

  for (const decl of decls) {
    matchVariant(decl, {
      Function: ({ name, funTy }) => {
        types.push(name.original + ': ' + PolyTy.show(PolyTy.canonicalize(funTy)));
      },
      TypeAlias: ({ name, typeParams, alias }) => {
        types.push(`type ${name}${TypeParams.show(typeParams)} = ${MonoTy.show(alias)}`);
      },
      _: () => { },
    });
  }

  return types;
};

const [, , source, debugLvl] = process.argv;

(async () => {
  if (source) {
    if (debugLvl) {
      debugLevel = parseInt(debugLvl);
    }
    const allGood = await run(source);
    process.exit(allGood ? 0 : 1);
  } else {
    console.info('Usage: yo <source.yo>');
    process.exit(0);
  }
})();
