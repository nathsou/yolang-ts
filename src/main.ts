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

const typeCheck = (sweetProg: SweetProg): [Prog, TypeContext, Error[]] => {
  Context.clear();
  const errors: Error[] = [];

  if (debugLevel >= DebugLvl.sweet) {
    console.log('--- sweet ---');
    console.log(SweetProg.show(sweetProg) + '\n');
  }

  const [prog, bitterErrors] = Prog.fromSweet(sweetProg);
  errors.push(...bitterErrors);

  const [typingErrors, typeCtx] = infer(prog);
  errors.push(...typingErrors);

  return [prog, typeCtx, errors];
};

const runWasmFile = async (source: string): Promise<Function> => {
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
  const nfs = await createNodeFileSystem();
  const [sweetProg, errs1] = await resolve(source, nfs);
  const [prog, _, errs2] = typeCheck(sweetProg);
  const errors = [...errs1, ...errs2];

  errors.forEach(err => {
    console.log('\x1b[31m%s\x1b[0m', Error.show(err));
  });

  if (errors.length === 0) {
    if (debugLevel >= DebugLvl.types) {
      console.log('--- types ---');
      console.log(showTypes(prog, []).join('\n\n') + '\n');
    }

    if (prog.length === 1 && prog[0].variant === 'Module') {
      const compiler = await createLLVMCompiler();
      const module = compiler.compile(prog[0]);
      module.setSourceFileName(source);

      if (debugLevel >= DebugLvl.sections) {
        console.log(module.print());
      }

      const outPath = 'out';

      await compiler.compileIR(module, outPath, 3);
      const mainFunc = await runWasmFile(`${outPath}.wasm`);

      console.log(mainFunc());
    } else {
      panic('no entry module found');
    }
  }

  return errors.length === 0;
};

const showTypes = (decls: Decl[], path: string[]): string[] => {
  const types: string[] = [];
  const pathName = path.length > 0 ? path.join('.') + '.' : '';

  for (const decl of decls) {
    matchVariant(decl, {
      Function: ({ name, funTy }) => {
        types.push(pathName + name.original + ': ' + PolyTy.show(PolyTy.canonicalize(funTy)));
      },
      Module: mod => {
        types.push(...showTypes(mod.decls, [...path, mod.name]));
      },
      TypeAlias: ({ name, typeParams, alias }) => {
        types.push(`type ${pathName}${name}${TypeParams.show(typeParams)} = ${MonoTy.show(alias)}`);
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