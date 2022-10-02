import { exec } from 'child_process';
import { sync as commandExists } from 'command-exists';
import { existsSync, mkdirSync } from 'fs';
import type LLVM from 'llvm-bindings';
import llvm from 'llvm-bindings';
import { matchString, panic } from '../../utils/misc';

export const buildLLVM = async (
  modules: Map<string, LLVM.Module>,
  target: 'host' | 'wasm' | 'wasi',
  outDir: string,
  outFile: string,
  optLevel: 0 | 1 | 2 | 3
): Promise<string> => {
  const findCommand = (name: string, cmds: string[]): string => {
    for (const cmd of cmds) {
      if (commandExists(cmd)) {
        return cmd;
      }
    }

    return panic(`Could not find the '${name}' command, tried ${cmds.map(c => `'${c}'`).join(', ')}`);
  };

  if (!existsSync(outDir)) {
    mkdirSync(outDir);
  }

  const byteCodeFiles: string[] = [];

  const execPromise = (cmd: string): Promise<string> => {
    return new Promise((resolve, reject) => {
      exec(cmd, (error, stdout) => {
        if (error != null) {
          reject(error);
        } else {
          resolve(stdout);
        }
      });
    });
  };

  const targetTriple = matchString(target, {
    host: () => llvm.config.LLVM_DEFAULT_TARGET_TRIPLE,
    wasm: () => 'wasm32-unknown-unknown',
    wasi: () => 'wasm32-unknown-wasi',
  });

  modules.forEach(module => {
    module.setTargetTriple(targetTriple);
    const modOutFile = `${outDir}/${module.getName()}.bc`;
    llvm.WriteBitcodeToFile(module, modOutFile);
    byteCodeFiles.push(modOutFile);
  });

  const llvmVersion = 14;
  const llvmLinkCmd = findCommand('llvm-link', [`llvm-link-${llvmVersion}`, 'llvm-link']);
  const clangCmd = findCommand('clang', [`clang-${llvmVersion}`, 'clang']);

  // link all the modules together
  const linkedFile = `${outDir}/main.linked.bc`;
  const linkCommand = [
    llvmLinkCmd,
    ...byteCodeFiles,
    '-o',
    linkedFile,
  ].join(' ');

  const WASM_STACK_SIZE = 8 * 1024 * 1024;

  const buildCommand = matchString(target, {
    host: () => [
      clangCmd,
      `--target=${targetTriple}`,
      `-O${optLevel}`,
      `-o ${outFile}`,
      linkedFile,
    ],
    wasm: () => [
      clangCmd,
      `--target=${targetTriple}`,
      '-nostdlib',
      `-O${optLevel}`,
      '-Wl,--no-entry',
      '-Wl,--export-all',
      `-Wl,--lto-O${optLevel}`,
      '-Wl,--allow-undefined',
      '-flto', // Add metadata for link-time optimizations
      '-Wl,--lto-O3', // Aggressive link-time optimizations
      `-Wl,-z,stack-size=${WASM_STACK_SIZE}`, // Set maximum stack size to 8MiB
      `-o ${outFile}`,
      linkedFile,
    ],
    wasi: () => [
      clangCmd,
      linkedFile,
      `--target=${targetTriple}`,
      '-nostdlib',
      '-e main',
      '--sysroot=./build/wasi/wasi-sysroot',
      '-lc',
      './build/wasi/libclang/wasi/libclang_rt.builtins-wasm32.a',
      `-O${optLevel}`,
      '-flto', // Add metadata for link-time optimizations
      '-Wl,--lto-O3', // Aggressive link-time optimizations
      `-Wl,-z,stack-size=${WASM_STACK_SIZE}`, // Set maximum stack size to 8MiB
      `-o ${outFile}`,
    ],
  });

  const linkOutput = await execPromise(linkCommand);
  const buildOutput = await execPromise(buildCommand.join(' '));
  const stdout: string[] = [];

  if (linkOutput.length > 0) {
    stdout.push(linkOutput);
  }

  if (buildOutput.length > 0) {
    stdout.push(buildOutput);
  }

  return stdout.join('\n');
};
