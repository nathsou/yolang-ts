import { match as matchVariant } from "itsamatch";
import { Decl, Prog } from "./ast/bitter";
import { Context } from './ast/context';
import { Prog as SweetProg } from "./ast/sweet";
import { Compiler } from "./codegen/codegen";
import { Module } from "./codegen/wasm/sections";
import { Error } from './errors/errors';
import { infer } from "./infer/infer";
import { MonoTy, PolyTy, TypeParams } from "./infer/types";
import { createNodeFileSystem } from './resolve/nodefs';
import { resolve } from './resolve/resolve';

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

const typeCheck = (sweetProg: SweetProg): [Prog, Error[]] => {
  Context.clear();
  const errors: Error[] = [];

  if (debugLevel >= DebugLvl.sweet) {
    console.log('--- sweet ---');
    console.log(SweetProg.show(sweetProg) + '\n');
  }

  const [prog, bitterErrors] = Prog.fromSweet(sweetProg);
  errors.push(...bitterErrors);

  const typingErrors = infer(prog);
  errors.push(...typingErrors);

  return [prog, errors];
};

const run = async (source: string): Promise<boolean> => {
  const nfs = await createNodeFileSystem();
  const [sweetProg, errs1] = await resolve(source, nfs);
  const [prog, errs2] = typeCheck(sweetProg);
  const errors = [...errs1, ...errs2];

  errors.forEach(err => {
    console.log('\x1b[31m%s\x1b[0m', Error.show(err));
  });

  if (debugLevel >= DebugLvl.types) {
    console.log('--- types ---');
    console.log(showTypes(prog, []).join('\n\n') + '\n');
  }

  if (errors.length === 0) {
    const mod = Compiler.compile(prog);

    if (debugLevel >= DebugLvl.sections) {
      console.log('--- sections ---');
      console.log(Module.show(mod));
      // writeFileSync('out.wasm', Module.encodeUin8Array(mod), { encoding: 'binary' });
    }

    const compiled = new WebAssembly.Module(Module.encodeUin8Array(mod));
    const instance = new WebAssembly.Instance(compiled, {});

    const mainFunc = Object
      .entries(instance.exports)
      .find(([name, val]) => name.endsWith('_main') && val instanceof Function);

    if (mainFunc) {
      console.log((mainFunc[1] as Function)());
    } else {
      console.log('No main function found');
      return false;
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