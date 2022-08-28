import { match as matchVariant } from "itsamatch";
import * as bitter from "./ast/bitter";
import { Context } from './ast/context';
import * as core from "./ast/core";
import { Mono } from "./ast/monomorphize";
import * as sweet from "./ast/sweet";
import { createLLVMCompiler } from "./codegen/llvm/codegen";
import { Error } from './errors/errors';
import { infer } from "./infer/infer";
import { MonoTy, PolyTy, TypeParams } from "./infer/types";
import { createNodeFileSystem } from './resolve/nodefs';
import { resolve } from './resolve/resolve';
import { sum } from "./utils/array";
import { block, panic } from "./utils/misc";

enum DebugMode {
  run = 0,
  types = 1,
  sweet = 2,
  bitter = 3,
  mono = 4,
  llvm = 5,
  time = 6,
}

let debugMode = DebugMode.run;

// pipeline:
// <string> -> parse -> <sweet> -> desugar -> <bitter> -> infer ->
// monomorphize -> inject reference counting -> <core> -> codegen

const typeCheck = (prog: bitter.Prog): Error[] => {
  Context.clear();
  return infer(prog);
};

const getWasmMainFunc = async (source: string): Promise<() => number> => {
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
    return instance.exports['main'] as (() => number);
  }

  return panic('main function not found');
};

const time = <T>(fn: () => T): [T, number] => {
  const t1 = Date.now();
  const res = fn();
  return [res, Date.now() - t1];
};

const timeAsync = async <T>(fn: () => Promise<T>): Promise<[T, number]> => {
  const t1 = Date.now();
  const res = await fn();
  return [res, Date.now() - t1];
};

const compile = async (source: string, target: 'wasm' | 'native', opt: 0 | 1 | 2 | 3): Promise<number> => {
  const logErrors = (errors: Error[]) => {
    errors.forEach(err => {
      console.log('\x1b[31m%s\x1b[0m', Error.show(err));
    });
  };

  const nfs = await createNodeFileSystem();
  const [[sweetProg, errs1], resolveDuration] = await timeAsync(() => resolve(source, nfs));

  if (debugMode === DebugMode.sweet) {
    console.log('--- sweet ---');
    console.log(sweet.Prog.show(sweetProg) + '\n');
  }

  if (errs1.length > 0) {
    logErrors(errs1);
    return 1;
  }

  const [[bitterProg, errs2], bitterDuration] = time(() => bitter.Prog.from(sweetProg));

  if (debugMode === DebugMode.bitter) {
    console.log('--- bitter ---');
    console.log(bitter.Prog.show(bitterProg) + '\n');
  }

  if (errs2.length > 0) {
    logErrors(errs2);
    return 1;
  }

  const [errs3, inferDuration] = time(() => typeCheck(bitterProg));

  if (errs3.length > 0) {
    logErrors(errs3);
    return 1;
  }

  if (debugMode === DebugMode.types) {
    console.log('--- types ---');
    console.log(showTypes(bitterProg.entry.decls).join('\n\n') + '\n');
  }

  const [[monoProg, instances, errs4], monoDuration] = time(() => Mono.prog(bitterProg));

  if (debugMode === DebugMode.mono) {
    console.log('--- mono ---');
    console.log(bitter.Prog.show(monoProg) + '\n');
  }

  if (errs4.length > 0) {
    logErrors(errs4);
    return 1;
  }

  const [[coreProg, errs5], coreDuration] = time(() => core.Prog.from(monoProg, instances));

  if (errs5.length > 0) {
    logErrors(errs5);
    return 1;
  }

  const compiler = createLLVMCompiler();
  const [modules, compileDuration] = time(() => compiler.compile(coreProg));

  if (debugMode === DebugMode.llvm) {
    modules.forEach(m => {
      console.log(m.print());
    });
  }

  const outDir = 'out';
  const outFile = target === 'wasm' ? `${outDir}/main.wasm` : `${outDir}/main`;
  const [stdout, buildDuration] = await timeAsync(() => compiler.compileIR(modules, target, outDir, outFile, opt));

  if (stdout.length > 0) {
    console.log(stdout);
  }

  if (debugMode === DebugMode.run) {
    const exitCode = await block(async () => {
      if (target === 'wasm') {
        // run the program
        const mainFunc = await getWasmMainFunc(outFile);
        return mainFunc();
      } else {
        const { execFileSync } = await import('child_process');
        try {
          const stdout = execFileSync(outFile);
          // remove trailing newline
          const filteredStdout = stdout.at(-1) === 10 ? stdout.slice(0, -1) : stdout;
          console.log(filteredStdout.toString('utf-8'));
          return 0;
        } catch (err: any) {
          console.error(err);
          return err.status;
        }
      }
    });

    return exitCode;
  }

  if (debugMode === DebugMode.time) {
    const durations = {
      'parsing': resolveDuration,
      'sweet -> bitter': bitterDuration,
      'type inference': inferDuration,
      'monomorphization': monoDuration,
      'bitter -> core': coreDuration,
      'compilation': compileDuration,
      'build': buildDuration,
    };

    console.log('--- timing ---');
    Object.entries(durations).forEach(([name, duration]) => {
      console.log(`${name}: ${duration}ms`);
    });

    console.log(`total: ${sum(Object.values(durations))}ms`);
  }

  return 0;
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
      debugMode = parseInt(debugLvl);
    }
    const exitCode = await compile(source, 'native', 3);
    process.exit(exitCode);
  } else {
    console.info('Usage: yo <source.yo>');
    process.exit(0);
  }
})();
