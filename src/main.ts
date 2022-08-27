import { match as matchVariant } from "itsamatch";
import * as bitter from "./ast/bitter";
import { Context } from './ast/context';
import * as core from "./ast/core";
import * as sweet from "./ast/sweet";
import { createLLVMCompiler } from "./codegen/llvm/codegen";
import { Error } from './errors/errors';
import { infer } from "./infer/infer";
import { MonoTy, PolyTy, TypeParams } from "./infer/types";
import { createNodeFileSystem } from './resolve/nodefs';
import { resolve } from './resolve/resolve';
import { panic } from "./utils/misc";

enum DebugLvl {
  nothing = 0,
  sections = 1,
  types = 2,
  sweet = 3,
  llvm = 4,
  time = 5,
  all = 6,
}

let debugLevel = DebugLvl.nothing;

// pipeline:
// <string> -> parse -> <sweet> -> desugar & resolve modules -> <bitter> -> infer ->
// monomorphize -> inject reference counting -> emit code

const typeCheck = (prog: bitter.Prog): Error[] => {
  Context.clear();
  return infer(prog);
};

const getWasmMainFunc = async (source: string): Promise<Function> => {
  const { readFile } = await import('fs/promises');

  const wasm = await readFile(source);
  const module = new WebAssembly.Module(wasm);
  const putcharBuffer: number[] = [];
  const utf8Decoder = new TextDecoder('utf-8');
  const instance = new WebAssembly.Instance(module, {
    env: {
      log: (chars: number, len: number): void => {
        const mem = instance.exports.memory as WebAssembly.Memory;
        console.log(utf8Decoder.decode(mem.buffer.slice(chars, chars + len)));
      },
      putchar: (char: number): number => {
        if (char === 10) { // \n
          console.log(utf8Decoder.decode(Uint8Array.from(putcharBuffer)));
          putcharBuffer.length = 0;
        } else {
          putcharBuffer.push(char);
        }

        return char;
      },
    }
  });

  if ('main' in instance.exports && typeof instance.exports['main'] === 'function') {
    return instance.exports['main'];
  }

  return panic('main function not found');
};

const compile = async (source: string, target: 'wasm' | 'native', opt: 0 | 1 | 2 | 3): Promise<number> => {
  const logErrors = (errors: Error[]) => {
    errors.forEach(err => {
      console.log('\x1b[31m%s\x1b[0m', Error.show(err));
    });
  };

  const nfs = await createNodeFileSystem();
  const [sweetProg, errs1] = await resolve(source, nfs);

  if (errs1.length > 0) {
    logErrors(errs1);
    return 1;
  }

  if (debugLevel >= DebugLvl.sweet) {
    console.log('--- sweet ---');
    console.log(sweet.Prog.show(sweetProg) + '\n');
  }

  const [bitterProg, errs2] = bitter.Prog.from(sweetProg);

  if (errs2.length > 0) {
    logErrors(errs2);
    return 1;
  }

  const errs3 = typeCheck(bitterProg);

  if (errs3.length > 0) {
    logErrors(errs3);
    return 1;
  }

  if (debugLevel >= DebugLvl.types) {
    console.log('--- types ---');
    console.log(showTypes(bitterProg.entry.decls).join('\n\n') + '\n');
  }

  const [coreProg, errs4] = core.Prog.from(bitterProg);

  if (errs4.length > 0) {
    logErrors(errs4);
    return 1;
  }

  const compiler = await createLLVMCompiler();
  const modules = compiler.compile(coreProg);

  if (debugLevel >= DebugLvl.llvm) {
    modules.forEach(m => {
      console.log(m.print());
    });
  }

  const outDir = 'out';
  const outFile = target === 'wasm' ? `${outDir}/main.wasm` : `${outDir}/main`;
  const stdout = await compiler.compileIR(modules, target, outDir, outFile, opt);

  if (stdout.length > 0) {
    console.log(stdout);
  }

  let t1, t2 = 0;
  let exitCode = 0;
  if (target === 'wasm') {
    // run the program
    const mainFunc = await getWasmMainFunc(outFile);
    t1 = Date.now();
    exitCode = mainFunc();
    t2 = Date.now();
  } else {
    const { execFileSync } = await import('child_process');
    t1 = Date.now();
    const stdout = execFileSync(outFile);
    t2 = Date.now();
    // remove trailing newline
    try {
      const filteredStdout = stdout.at(-1) === 10 ? stdout.slice(0, -1) : stdout;
      console.log(filteredStdout.toString('utf-8'));
      exitCode = 0;
    } catch (err: any) {
      exitCode = err.status;
    }
  }

  if (debugLevel >= DebugLvl.time) {
    console.log(`took ${t2 - t1}ms`);
  }

  return exitCode;
};

const showTypes = (decls: bitter.Decl[]): string[] => {
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
    const exitCode = await compile(source, 'native', 3);
    process.exit(exitCode);
  } else {
    console.info('Usage: yo <source.yo>');
    process.exit(0);
  }
})();
