import { program } from 'commander';
import { match } from "itsamatch";
import { resolve as resolvePath } from 'path';
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
import { block, matchString, panic } from "./utils/misc";
import { spawn, execFileSync } from 'child_process';

const TARGETS = ['host', 'wasm', 'wasi'] as const;

type Options = {
  out?: string,
  target: (typeof TARGETS)[number],
  run: boolean,
  typeCheck: boolean,
  artifacts: string,
  debug: boolean,
  'show:types': boolean,
  'show:sweet': boolean,
  'show:bitter': boolean,
  'show:mono': boolean,
  'show:llvm': boolean,
  'show:time': boolean,
}

program
  .name('yolang')
  .version('0.0.1')
  .argument('<input>', 'input source file')
  .option('-o, --out <file>', 'output file')
  .option('-t, --target <name>', `target: ${TARGETS.join(' | ')}`, TARGETS[0])
  .option('-r, --run', 'compile and run the program')
  .option('-a, --artifacts', 'build artificats directory', 'out')
  .option('-T, --type-check', 'type check only')
  .option('-D, --debug', 'create a debug build')
  .option('--show:types', 'show types')
  .option('--show:sweet', 'show sweet IR')
  .option('--show:bitter', 'show bitter IR')
  .option('--show:mono', 'show the monomorphized program')
  .option('--show:llvm', 'show LLVM IR')
  .option('--show:time', 'show pipeline timing')
  .action(input => {
    const opts = program.opts<Options>();

    if (!TARGETS.includes(opts.target)) {
      console.error(`invalid target '${opts.target}', options are: ${TARGETS.join(', ')}`);
      process.exit(1);
    }

    yo(input, opts)
      .then(exitCode => {
        process.exit(exitCode);
      })
      .catch(err => {
        console.error(err);
      });
  });

program.parse();

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

async function yo(source: string, options: Options): Promise<number> {
  const nfs = await createNodeFileSystem();

  const logErrors = async (errors: Error[]) => {
    let index = 1;
    for (const err of errors) {
      console.log('\x1b[31m%s\x1b[0m', (errors.length > 1 ? `[${index} / ${errors.length}] ` : '') + await Error.show(err, nfs), '\n');
      index += 1;
    }
  };

  const [[sweetProg, errs1], resolveDuration] = await timeAsync(() => resolve(source, nfs));

  if (options['show:sweet']) {
    console.log('--- sweet ---');
    console.log(sweet.Prog.show(sweetProg) + '\n');
  }

  if (errs1.length > 0) {
    await logErrors(errs1);
    return 1;
  }

  const [[bitterProg, errs2], bitterDuration] = time(() => bitter.Prog.from(sweetProg));

  if (options['show:bitter']) {
    console.log('--- bitter ---');
    console.log(bitter.Prog.show(bitterProg) + '\n');
  }

  if (errs2.length > 0) {
    await logErrors(errs2);
    return 1;
  }

  const [errs3, inferDuration] = time(() => typeCheck(bitterProg));

  if (errs3.length > 0) {
    await logErrors(errs3);
    return 1;
  }

  if (options.typeCheck) {
    return 0;
  }

  if (options['show:types']) {
    console.log('--- types ---');
    console.log(showTypes(bitterProg.entry.decls).join('\n\n') + '\n');
  }

  const [[monoProg, instances, errs4], monoDuration] = time(() => Mono.prog(bitterProg));

  if (options['show:mono']) {
    console.log('--- mono ---');
    console.log(bitter.Prog.show(monoProg) + '\n');
  }

  if (errs4.length > 0) {
    await logErrors(errs4);
    return 1;
  }

  const [[coreProg, errs5], coreDuration] = time(() => core.Prog.from(monoProg, instances));

  if (errs5.length > 0) {
    await logErrors(errs5);
    return 1;
  }

  const compiler = createLLVMCompiler();
  const [modules, compileDuration] = time(() => compiler.compile(coreProg));

  if (options['show:llvm']) {
    modules.forEach(m => {
      console.log(m.print());
    });
  }

  const outFile = resolvePath(block(() => {
    if (options.out != null) {
      return options.out;
    }

    return matchString(options.target, {
      host: () => 'main',
      wasm: () => 'main.wasm',
      wasi: () => 'main.wasm',
    });
  }));

  const [stdout, buildDuration] = await timeAsync(() => compiler.compileIR(
    modules,
    options.target,
    options.artifacts,
    outFile,
    options.debug ? 0 : 3,
  ));

  if (stdout.length > 0) {
    console.log(stdout);
  }

  if (options['show:time']) {
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

  if (options.run) {
    const spawnPromise = (cmd: string, ...args: string[]) => new Promise<void>(resolve => {
      const sp = spawn(cmd, args);

      sp.on('error', err => {
        console.error(err);
      });

      sp.stdout.on('data', data => {
        process.stdout.write(data.toString());
      });

      sp.stderr.on('data', data => {
        process.stderr.write(data.toString());
      });

      sp.on('close', () => {
        resolve();
      });
    });

    const exitCode = await block(async () => {
      switch (options.target) {
        case 'wasm':
          // run the program
          const mainFunc = await getWasmMainFunc(outFile);
          return mainFunc();
        case 'wasi':
          await spawnPromise('wasmtime', outFile, '--invoke', 'main');
          return 0;
        case 'host':
          try {
            process.stdout.write(execFileSync(outFile));
            return 0;
          } catch (err: any) {
            console.error(err);
            return err.status;
          }

      }
    });

    return exitCode;
  }

  return 0;
}

function showTypes(decls: bitter.Decl[]): string[] {
  const types: string[] = [];

  for (const decl of decls) {
    match(decl, {
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
}
