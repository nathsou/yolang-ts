import { match, VariantOf } from 'itsamatch';
import type LLVM from 'llvm-bindings';
import { ArrayInit, Decl, Expr, Module, Prog, Stmt } from '../../ast/bitter';
import { VarName } from '../../ast/name';
import * as sweet from '../../ast/sweet';
import { Row } from '../../infer/structs';
import { TypeContext } from '../../infer/typeContext';
import { MonoTy, PolyTy } from '../../infer/types';
import { Const } from '../../parse/token';
import { gen, last, zip } from '../../utils/array';
import { Maybe } from '../../utils/maybe';
import { assert, matchString, panic, proj } from '../../utils/misc';
import { meta } from './attributes';

type LocalVar =
  | { kind: 'mut', name: string, ptr: LLVM.AllocaInst, ty: MonoTy }
  | { kind: 'immut', name: string, value: LLVM.Value, ty: MonoTy };

type LexicalScope = Record<string, LocalVar>;
const LexicalScope = {
  make: (): LexicalScope => ({}),
  declareImmutableVar: (self: LexicalScope, name: string, value: LLVM.Value, ty: MonoTy): void => {
    self[name] = { kind: 'immut', name, value, ty };
  },
  declareMutableVar: (self: LexicalScope, name: string, ptr: LLVM.AllocaInst, ty: MonoTy): void => {
    self[name] = { kind: 'mut', name, ptr, ty };
  },
  resolve: (self: LexicalScope, name: string): LocalVar => {
    return self[name];
  },
  has: (self: LexicalScope, name: string): boolean => name in self,
};

export const createLLVMCompiler = async () => {
  const llvm = await import('llvm-bindings');

  function compileModule(module: Module, modules: Map<string, Module>): LLVM.Module {
    const context = new llvm.LLVMContext();
    const mod = new llvm.Module(module.path, context);
    mod.setModuleIdentifier(module.name);
    mod.setSourceFileName(module.path);
    const builder = new llvm.IRBuilder(context);
    const scopes: LexicalScope[] = [];
    const unit = Expr.Const(Const.unit(), sweet.Expr.Const(Const.unit()));
    const unitTy = MonoTy.Const('()');
    const funcs = new Map<string, LLVM.Function>();

    type ResolvedVar =
      | { kind: 'immut', value: LLVM.Value, ty: MonoTy }
      | { kind: 'mut', ptr: LLVM.AllocaInst, ty: MonoTy };

    function resolveVar(name: VarName): ResolvedVar {
      for (let i = scopes.length - 1; i >= 0; i--) {
        const scope = scopes[i];
        if (LexicalScope.has(scope, name.mangled)) {
          const v = LexicalScope.resolve(scope, name.mangled);
          if (v.kind === 'immut') {
            return { kind: 'immut', value: v.value, ty: v.ty };
          } else {
            return { kind: 'mut', ptr: v.ptr, ty: v.ty };
          }
        }
      }

      return panic(`undeclared variable '${name.mangled}'`);
    }

    function currentScope(): LexicalScope {
      if (scopes.length === 0) {
        panic('scope list is empty');
      }

      return last(scopes);
    }

    function declareVar(name: VarName, value: LLVM.Value): void {
      if (name.mutable) {
        const parent = builder.GetInsertBlock()!.getParent()!;
        const tmpBuilder = new llvm.IRBuilder(parent.getEntryBlock());
        const alloca = tmpBuilder.CreateAlloca(llvmTy(name.ty), tmpBuilder.getInt32(1), name.mangled);
        LexicalScope.declareMutableVar(currentScope(), name.mangled, alloca, name.ty);
        builder.CreateStore(value, alloca);
      } else {
        LexicalScope.declareImmutableVar(currentScope(), name.mangled, value, name.ty);
      }
    }

    function structFieldPtr(struct: VariantOf<MonoTy, 'Struct'>, structPtr: LLVM.Value, field: string): LLVM.Value {
      const index = Row.fields(struct.row).findIndex(([name]) => name === field);
      assert(index >= 0);
      const fieldIndex = builder.getInt32(index);
      return builder.CreateInBoundsGEP(
        llvmTy(struct).getPointerElementType(),
        structPtr,
        [builder.getInt32(0), fieldIndex],
        field,
      );
    }

    function arrayElementPtr(ty: LLVM.Type, arrayPtr: LLVM.Value, index: number): LLVM.Value {
      return builder.CreateInBoundsGEP(
        ty,
        arrayPtr,
        [builder.getInt32(0), builder.getInt32(index)],
      );
    }

    function createUnit(): LLVM.Value {
      return compileExpr(unit);
    }

    function resolveStruct(name: string, typeParams: MonoTy[]): Maybe<[struct: VariantOf<MonoTy, 'Struct'>, ty: LLVM.StructType]> {
      const ta = TypeContext.resolveTypeAlias(module.typeContext, name);

      return ta.map(({ ty, params }) => {
        assert(ty.variant === 'Struct');
        const subst = new Map(zip(params.map(proj('name')), typeParams));
        const struct = MonoTy.substituteTyParams(ty, subst) as VariantOf<MonoTy, 'Struct'>;
        assert(struct.variant === 'Struct');
        const structTy = llvmTy(struct).getPointerElementType() as LLVM.StructType;
        assert(structTy.isStructTy());

        return [struct, structTy];
      });
    }

    function compileExpr(expr: Expr): llvm.Value {
      return match(expr, {
        Const: ({ value: c }) => match(c, {
          bool: ({ value }) => llvm.ConstantInt[value ? 'getTrue' : 'getFalse'](context),
          u32: ({ value }) => llvm.ConstantInt.get(llvm.Type.getInt32Ty(context), value),
          i32: ({ value }) => llvm.ConstantInt.get(llvm.Type.getInt32Ty(context), value),
          u64: ({ value }) => llvm.ConstantInt.get(llvm.Type.getInt64Ty(context), value),
          i64: ({ value }) => llvm.ConstantInt.get(llvm.Type.getInt64Ty(context), value),
          i8: ({ value }) => llvm.ConstantInt.get(llvm.Type.getInt8Ty(context), value),
          u8: ({ value }) => llvm.ConstantInt.get(llvm.Type.getInt8Ty(context), value),
          unit: () => llvm.UndefValue.get(llvm.Type.getInt1Ty(context)),
        }),
        Variable: ({ name }) => {
          const v = resolveVar(name);

          if (v.kind === 'immut') {
            return v.value;
          } else {
            return builder.CreateLoad(llvmTy(v.ty), v.ptr);
          }
        },
        Block: ({ statements, lastExpr }) => {
          return scoped(() => {
            statements.forEach(stmt => {
              compileStmt(stmt);
            });

            return compileExpr(lastExpr.orDefault(unit));
          });
        },
        NamedFuncCall: ({ name, args }) => {
          const funcName = name.unwrapRight('codegen: unresolved function name');
          if (funcs.has(funcName.mangled)) {
            return builder.CreateCall(funcs.get(funcName.mangled)!, args.map(arg => compileExpr(arg)));
          } else {
            return panic(`undeclared function in call: ${funcName.mangled}`);
          }
        },
        IfThenElse: ({ condition, then, else_, ty }) => {
          const parent = builder.GetInsertBlock()!.getParent()!;
          const condValue = compileExpr(condition);

          let thenBB = llvm.BasicBlock.Create(context, 'then', parent);
          let elseBB = llvm.BasicBlock.Create(context, 'else');
          const mergeBB = llvm.BasicBlock.Create(context, 'if_then');

          builder.CreateCondBr(condValue, thenBB, elseBB);

          // then branch
          builder.SetInsertPoint(thenBB);
          const thenValue = compileExpr(then);
          builder.CreateBr(mergeBB);
          // Codegen of 'then' can change the current block, update thenBB for the PHI. 
          thenBB = builder.GetInsertBlock()!;

          // else branch
          parent.insertAfter(thenBB, elseBB);
          builder.SetInsertPoint(elseBB);
          builder.SetInsertPoint(elseBB);
          const elseValue = compileExpr(else_.orDefault(unit));
          builder.CreateBr(mergeBB);
          elseBB = builder.GetInsertBlock()!;

          // merge
          parent.insertAfter(elseBB, mergeBB);
          builder.SetInsertPoint(mergeBB);

          if (!MonoTy.eq(ty, unitTy)) {
            const phiNode = builder.CreatePHI(llvmTy(ty), 2, 'if');
            phiNode.addIncoming(thenValue, thenBB);
            phiNode.addIncoming(elseValue, elseBB);

            return phiNode;
          } else {
            return createUnit();
          }
        },
        While: ({ condition, body }) => {
          const parent = builder.GetInsertBlock()!.getParent()!;
          const condValue = compileExpr(condition);

          const loopBB = llvm.BasicBlock.Create(context, 'loop', parent);
          const loopEndBB = llvm.BasicBlock.Create(context, 'loop_end');

          builder.CreateCondBr(condValue, loopBB, loopEndBB);

          builder.SetInsertPoint(loopBB);
          compileExpr(body);
          const condValue2 = compileExpr(condition);
          builder.CreateCondBr(condValue2, loopBB, loopEndBB);

          parent.insertAfter(loopBB, loopEndBB);
          builder.SetInsertPoint(loopEndBB);

          return createUnit();
        },
        Assignment: ({ lhs, rhs }) => {
          return match(lhs, {
            Variable: ({ name }) => {
              const v = resolveVar(name);
              assert(v.kind === 'mut');
              builder.CreateStore(compileExpr(rhs), v.ptr);
              return createUnit();
            },
            FieldAccess: ({ lhs, field }) => {
              const structTy = MonoTy.deref(lhs.ty);
              assert(structTy.variant === 'Struct');
              const structPtr = compileExpr(lhs);
              const fieldPtr = structFieldPtr(structTy, structPtr, field);
              const value = compileExpr(rhs);

              builder.CreateStore(value, fieldPtr);
              return createUnit();
            },
            _: () => panic('unhandled assignment target: ' + Expr.showSweet(lhs)),
          });
        },
        Struct: ({ name, fields, typeParams }) => {
          const [struct, structTy] = resolveStruct(name, typeParams).unwrap('resolveStruct');
          const structPtr = builder.CreateAlloca(structTy, null, name);

          fields.forEach(({ name, value }) => {
            const fieldPtr = structFieldPtr(struct, structPtr, name);
            builder.CreateStore(compileExpr(value), fieldPtr);
          });

          return structPtr;
        },
        FieldAccess: ({ lhs, field, ty }) => {
          const structTy = MonoTy.deref(lhs.ty);
          assert(structTy.variant === 'Struct');
          const structPtr = compileExpr(lhs);
          const fieldPtr = structFieldPtr(structTy, structPtr, field);

          return builder.CreateLoad(llvmTy(ty), fieldPtr)
        },
        Array: ({ init, elemTy }) => {
          const elemTyLlvm = llvmTy(elemTy);
          const len = ArrayInit.len(init);
          const arrayTy = llvm.ArrayType.get(elemTyLlvm, ArrayInit.len(init));
          const arrayData = builder.CreateAlloca(arrayTy, null);
          const arrayDataPtr = builder.CreateBitCast(arrayData, llvm.PointerType.get(elemTyLlvm, 0));
          const [arrayStruct, arrayStructTy] = resolveStruct('[]', [elemTy]).unwrap('resolveStruct array');
          const arrayStructPtr = builder.CreateAlloca(arrayStructTy, null);
          const dataFieldPtr = structFieldPtr(arrayStruct, arrayStructPtr, 'data');
          const lenFieldPtr = structFieldPtr(arrayStruct, arrayStructPtr, 'len');

          builder.CreateStore(arrayDataPtr, dataFieldPtr);
          builder.CreateStore(llvm.ConstantInt.get(llvm.Type.getInt32Ty(context), len), lenFieldPtr);

          // TODO: use a loop when len is large
          const elems = match(init, {
            elems: ({ elems }) => elems.map(elem => compileExpr(elem)),
            fill: ({ value, count }) => {
              const elem = compileExpr(value);
              return gen(count, () => elem);
            },
          });

          if (!(
            init.variant === 'fill' &&
            init.value.variant === 'Const' &&
            init.value.value.variant != 'unit' &&
            init.value.value.value === 0)
          ) {
            elems.forEach((elem, index) => {
              const elemPtr = arrayElementPtr(arrayTy, arrayData, index);
              builder.CreateStore(elem, elemPtr);
            });
          }

          return arrayStructPtr;
        },
        _: () => panic(expr.variant + ' not implemented'),
      });
    }

    function compileStmt(stmt: Stmt): void {
      match(stmt, {
        Expr: ({ expr }) => { compileExpr(expr); },
        Let: ({ name, expr }) => {
          declareVar(name, compileExpr(expr));
        },
        Error: ({ }) => { },
      });
    }

    const structInstances: { row: Row, ty: LLVM.StructType }[] = [];

    function llvmTy(ty: MonoTy): llvm.Type {
      return match(ty, {
        Const: c => matchString<string, llvm.Type>(c.name, {
          'int': () => llvm.Type.getInt32Ty(context),
          'u32': () => llvm.Type.getInt32Ty(context),
          'i32': () => llvm.Type.getInt32Ty(context),
          'u64': () => llvm.Type.getInt64Ty(context),
          'i64': () => llvm.Type.getInt64Ty(context),
          'u8': () => llvm.Type.getInt8Ty(context),
          'i8': () => llvm.Type.getInt8Ty(context),
          'bool': () => llvm.Type.getInt1Ty(context),
          '()': () => llvm.Type.getInt1Ty(context),
          'ptr': () => llvm.PointerType.get(llvmTy(c.args[0]), 0),
          _: () => {
            const typeAlias = TypeContext.resolveTypeAlias(module.typeContext, c.name).map(ta => {
              const t = TypeContext.instantiateTypeAlias(ta, c.args);
              return llvmTy(t);
            });

            return typeAlias.unwrap(`Unknown type representation for const type: ${c.name}, ${Object.values(module.typeContext.typeParamsEnv).map(MonoTy.show)}`);
          },
        }),
        Var: v => match(v.value, {
          Link: ({ to }) => llvmTy(MonoTy.deref(to)),
          Unbound: () => panic('llvmTy: found uninstantiated type variable'),
        }, 'kind'),
        Struct: s => {
          const fields = Row.fields(s.row);

          for (const inst of structInstances) {
            if (Row.strictEq(inst.row, s.row)) {
              return llvm.PointerType.get(inst.ty, 0);
            }
          }

          const structTy = llvm.StructType.create(context, fields.map(([_, t]) => llvmTy(t)), s.name ?? 'anon_struct');
          structInstances.push({ row: s.row, ty: structTy });

          return llvm.PointerType.get(structTy, 0);
        },
        Integer: () => llvm.Type.getInt32Ty(context),
        _: () => panic('llvmTy: ' + ty.variant + ' not implemented'),
      });
    }

    function scoped<T>(action: (scope: LexicalScope) => T): T {
      const scope = LexicalScope.make();
      scopes.push(scope);
      const ret = action(scope);
      scopes.pop();

      return ret;
    }

    function declareFunc(f: VariantOf<Decl, 'Function'>): LLVM.Function {
      const returnTy = llvmTy(f.returnTy.or(f.body.map(b => b.ty)).unwrap());
      const argTys = f.args.map(a => llvmTy(a.name.ty));
      const funcTy = llvm.FunctionType.get(returnTy, argTys, false);
      const isExternal =
        f.pub ||
        f.name.original === 'main' ||
        f.attributes.some(attr => attr.name === 'extern');
      const linkage = llvm.Function.LinkageTypes[isExternal ? 'ExternalLinkage' : 'PrivateLinkage'];
      const func = llvm.Function.Create(funcTy, linkage, f.name.mangled, mod);
      funcs.set(f.name.mangled, func);

      return func;
    }

    function compileDecl(decl: Decl): void {
      match(decl, {
        Function: f => {
          if (f.instances.length > 0) {
            f.instances.forEach(g => {
              compileDecl(g);
            });

            return;
          }

          if (PolyTy.isPolymorphic(f.funTy)) {
            return;
          }

          const func = declareFunc(f);

          if (f.attributes.some(attr => attr.name === 'extern')) {
            return;
          }

          const entry = llvm.BasicBlock.Create(context, 'entry', func);
          builder.SetInsertPoint(entry);
          const ret = scoped(() => {
            f.args.forEach((arg, index) => {
              const llvmArg = func.getArg(index);
              llvmArg.setName(arg.name.mangled);
              declareVar(arg.name, llvmArg);
            });

            return f.body.match({
              Some: body => compileExpr(body),
              None: () => {
                if (f.attributes.some(attr => attr.name === 'meta')) {
                  return meta(
                    f.attributes.find(attr => attr.name === 'meta')!.args[0],
                    func,
                    builder,
                    llvm,
                    context,
                  );
                }

                return panic(`Function prototype for '${f.name.original}' is missing a meta or extern attribute`);
              },
            });
          });

          builder.CreateRet(ret);

          if (llvm.verifyFunction(func)) {
            panic('function verification failed');
          }
        },
        TypeAlias: () => { },
        Import: ({ path, imports }) => {
          const importedMod = modules.get(path)!;

          const declareImport = (decl: VariantOf<Decl, 'Function' | 'TypeAlias'>) => {
            match(decl, {
              Function: f => {
                if (PolyTy.isPolymorphic(f.funTy)) {
                  f.instances.forEach(inst => {
                    declareFunc(inst);
                  });
                } else {
                  declareFunc(f);
                }
              },
              TypeAlias: () => { },
            });
          };

          match(imports, {
            names: ({ names }) => {
              names.forEach(name => {
                const decls = importedMod.members.get(name)!;
                decls.forEach(declareImport);
              });
            },
            all: () => {
              importedMod.members.forEach(decls => {
                decls.forEach(declareImport);
              });
            },
          });
        },
        _: () => { },
      });
    }

    module.decls.forEach(decl => { compileDecl(decl); });

    if (llvm.verifyModule(mod)) {
      panic('module verification failed');
    }

    return mod;
  }

  function compile(prog: Prog): Map<string, LLVM.Module> {
    const llvmModules = new Map<string, LLVM.Module>();

    prog.modules.forEach(mod => {
      const llvmModule = compileModule(mod, prog.modules);
      llvmModules.set(mod.path, llvmModule);
    });

    return llvmModules;
  }

  async function compileIR(
    modules: Map<string, LLVM.Module>,
    target: 'native' | 'wasm',
    outDir: string,
    outFile: string,
    optLevel: 0 | 1 | 2 | 3
  ): Promise<string> {
    const { exec } = await import('child_process');
    const { existsSync, mkdirSync } = await import('fs');

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
      wasm: () => 'wasm32-unknown-unknown',
      native: () => llvm.config.LLVM_DEFAULT_TARGET_TRIPLE,
    });

    modules.forEach(module => {
      module.setTargetTriple(targetTriple);
      const modOutFile = `${outDir}/${module.getName()}.bc`;
      llvm.WriteBitcodeToFile(module, modOutFile);
      byteCodeFiles.push(modOutFile);
    });

    // link all the modules together
    const linkedFile = `${outDir}/main.linked.bc`;
    const linkCommand = [
      'llvm-link',
      ...byteCodeFiles,
      '-o',
      linkedFile,
    ].join(' ');

    const buildCommand = matchString(target, {
      wasm: () => [
        'clang',
        `--target=${targetTriple}`,
        '-nostdlib',
        `-O${optLevel}`,
        '-Wl,--no-entry',
        '-Wl,--export-all',
        `-Wl,--lto-O${optLevel}`,
        '-Wl,--allow-undefined',
        `-o ${outFile}`,
        linkedFile,
      ],
      native: () => [
        'clang',
        `--target=${targetTriple}`,
        `-O${optLevel}`,
        `-o ${outFile}`,
        linkedFile,
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
  }

  return { compile, compileIR };
};
