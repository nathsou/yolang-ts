import { match, VariantOf } from 'itsamatch';
import type LLVM from 'llvm-bindings';
import llvm from 'llvm-bindings';
import { ArrayInit, Decl, Expr, Module, Prog, Stmt } from '../../ast/core';
import { VarName } from '../../ast/name';
import { Row } from '../../infer/structs';
import { TypeContext } from '../../infer/typeContext';
import { MonoTy } from '../../infer/types';
import { IntKind } from '../../parse/token';
import { last, zip } from '../../utils/array';
import { Maybe } from '../../utils/maybe';
import { array, assert, block, fst, getMap, matchString, panic, proj } from '../../utils/misc';
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

export const createLLVMCompiler = () => {
  function compileModule(module: Module, modules: Map<string, Module>): LLVM.Module {
    const context = new llvm.LLVMContext();
    const mod = new llvm.Module(module.path, context);
    mod.setModuleIdentifier(module.name);
    mod.setSourceFileName(module.path);
    const builder = new llvm.IRBuilder(context);
    const scopes: LexicalScope[] = [];
    const funcs = new Map<string, LLVM.Function>();
    const stacks = {
      func: array<{ returnVal?: llvm.AllocaInst, exitBB: llvm.BasicBlock }>(),
      controlFlow: array<{ exitBB: llvm.BasicBlock }>(),
    };

    function declareImports() {
      for (const [name, { sourceMod: sourceModPath }] of module.imports) {
        const sourceMod = modules.get(sourceModPath)!;
        const decls = sourceMod.members.get(name);
        decls?.forEach(d => {
          declareFunc(d, true);
        });
      }
    }

    // if a return statement is used inside a if then else or while branch
    // then we need to change the target branch to the return's exit basic block
    function setTargetControlFlowBranch<T>(targetBB: llvm.BasicBlock, action: () => T): [llvm.BasicBlock, T] {
      stacks.controlFlow.push({ exitBB: targetBB });
      const ret = action();
      const { exitBB } = stacks.controlFlow.pop()!;

      return [exitBB, ret];
    }

    type ResolvedVar =
      | { kind: 'mut', ptr: LLVM.AllocaInst, ty: MonoTy }
      | { kind: 'immut', value: LLVM.Value, ty: MonoTy };

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
        panic('Scope list is empty');
      }

      return last(scopes);
    }

    function declareVar(name: VarName, value: LLVM.Value): void {
      if (name.mutable) {
        const alloca = builder.CreateAlloca(llvmTy(name.ty), builder.getInt32(1), name.mangled);
        LexicalScope.declareMutableVar(currentScope(), name.mangled, alloca, name.ty);
        builder.CreateStore(value, alloca);
      } else {
        LexicalScope.declareImmutableVar(currentScope(), name.mangled, value, name.ty);
      }
    }

    const voidIndicator = 'void' as unknown as LLVM.Value;

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

    function arrayElementPtr(ty: LLVM.Type, arrayPtr: LLVM.Value, index: number | LLVM.Value): LLVM.Value {
      return builder.CreateInBoundsGEP(
        ty,
        arrayPtr,
        [builder.getInt32(0), typeof index === 'number' ? builder.getInt32(index) : index],
      );
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

    function allocateArray(elemTy: MonoTy, len: number) {
      const llvmElemTy = llvmTy(elemTy);
      const arrayTy = llvm.ArrayType.get(llvmElemTy, len);
      const arrayData = builder.CreateAlloca(arrayTy, null);
      const arrayDataPtr = builder.CreateBitCast(arrayData, llvm.PointerType.get(llvmElemTy, 0));
      const [arrayStruct, arrayStructTy] = resolveStruct('Array', [elemTy]).unwrap('resolveStruct array');
      const arrayStructPtr = builder.CreateAlloca(arrayStructTy, null);
      const dataFieldPtr = structFieldPtr(arrayStruct, arrayStructPtr, 'data');
      const lenFieldPtr = structFieldPtr(arrayStruct, arrayStructPtr, 'len');

      builder.CreateStore(arrayDataPtr, dataFieldPtr);
      builder.CreateStore(llvm.ConstantInt.get(llvm.Type.getInt32Ty(context), len), lenFieldPtr);

      return {
        arrayTy,
        arrayData,
        llvmElemTy,
        arrayStruct,
        lenFieldPtr,
        arrayDataPtr,
        dataFieldPtr,
        arrayStructTy,
        arrayStructPtr,
      };
    }

    function createArray(elemTy: MonoTy, elems: LLVM.Value[]): LLVM.AllocaInst {
      const { arrayTy, arrayData, arrayStructPtr } = allocateArray(elemTy, elems.length);

      elems.forEach((elem, index) => {
        const elemPtr = arrayElementPtr(arrayTy, arrayData, index);
        builder.CreateStore(elem, elemPtr);
      });

      return arrayStructPtr;
    }

    function createWhileLoop(compileCondition: () => LLVM.Value, compileBody: () => void): void {
      const parent = builder.GetInsertBlock()!.getParent()!;
      const loopCondBB = llvm.BasicBlock.Create(context, 'loop_cond', parent);
      const loopBodyBB = llvm.BasicBlock.Create(context, 'loop_body');
      const loopEndBB = llvm.BasicBlock.Create(context, 'loop_end');

      builder.CreateBr(loopCondBB);

      // loop condition
      builder.SetInsertPoint(loopCondBB);
      const condValue = compileCondition();
      builder.CreateCondBr(condValue, loopBodyBB, loopEndBB);

      // loop body
      parent.insertAfter(loopCondBB, loopBodyBB);
      builder.SetInsertPoint(loopBodyBB);
      const [loopExitBB] = setTargetControlFlowBranch(loopCondBB, compileBody);
      builder.CreateBr(loopExitBB);

      // loop end
      parent.insertAfter(loopBodyBB, loopEndBB);
      builder.SetInsertPoint(loopEndBB);
    }

    function compileExpr(expr: Expr): llvm.Value {
      return match(expr, {
        Const: ({ value: c }) => match(c, {
          bool: ({ value }) => llvm.ConstantInt[value ? 'getTrue' : 'getFalse'](context),
          int: ({ value, ty }) => llvm.ConstantInt.get(llvmTy(ty), value),
          str: ({ value }) => {
            const buffer = new TextEncoder().encode(value);
            const int8Ty = llvm.Type.getInt8Ty(context);
            const bytes = Array.from(buffer).map(b => llvm.ConstantInt.get(int8Ty, b));

            return createArray(MonoTy.int('u8'), bytes);
          },
        }),
        Variable: ({ name }) => {
          const v = resolveVar(name);

          if (v.kind === 'immut') {
            return v.value;
          } else {
            return builder.CreateLoad(llvmTy(v.ty), v.ptr);
          }
        },
        Block: ({ stmts, ret }) => {
          return scoped(() => {
            stmts.forEach(stmt => {
              compileStmt(stmt);
            });

            return ret.match({
              Some: compileExpr,
              None: () => voidIndicator,
            });
          });
        },
        NamedFuncCall: ({ name, args }) => {
          return getMap(funcs, name.mangled).match({
            Some: f => builder.CreateCall(f, args.map(arg => compileExpr(arg))),
            None: () => panic(`undeclared function in call: '${name.mangled}'`),
          });
        },
        IfThenElse: ({ condition, then, else_, ty }) => {
          const parent = builder.GetInsertBlock()!.getParent()!;
          const condValue = compileExpr(condition);
          let thenBB = llvm.BasicBlock.Create(context, 'then', parent);

          return else_.match({
            Some: elseExpr => {
              let elseBB = llvm.BasicBlock.Create(context, 'else');
              const mergeBB = llvm.BasicBlock.Create(context, 'merge');

              builder.CreateCondBr(condValue, thenBB, elseBB);

              // then branch
              builder.SetInsertPoint(thenBB);
              const thenValue = compileExpr(then);
              builder.CreateBr(mergeBB);
              // Codegen of 'then' can change the current block, update thenBB for the PHI. 
              // Codegen of 'then' can change the current block, update thenBB for the PHI. 
              // Codegen of 'then' can change the current block, update thenBB for the PHI. 
              thenBB = builder.GetInsertBlock()!;

              // else branch
              parent.insertAfter(thenBB, elseBB);
              builder.SetInsertPoint(elseBB);
              const [exitBB, elseValue] = setTargetControlFlowBranch(mergeBB, () => compileExpr(elseExpr));
              builder.CreateBr(exitBB);
              elseBB = builder.GetInsertBlock()!;

              // merge
              parent.insertAfter(elseBB, mergeBB);
              builder.SetInsertPoint(mergeBB);

              if (MonoTy.eq(ty, MonoTy.void())) {
                return voidIndicator;
              } else {
                const phiNode = builder.CreatePHI(llvmTy(ty), 2, 'if');
                phiNode.addIncoming(thenValue, thenBB);
                phiNode.addIncoming(elseValue, elseBB);

                return phiNode;
              }
            },
            None: () => {
              const afterBB = llvm.BasicBlock.Create(context, 'after');
              builder.CreateCondBr(condValue, thenBB, afterBB);
              parent.insertAfter(thenBB, afterBB);

              // then branch
              builder.SetInsertPoint(thenBB);
              const [exitBB] = setTargetControlFlowBranch(afterBB, () => compileExpr(then));
              builder.CreateBr(exitBB);
              // Codegen of 'then' can change the current block, update thenBB for the PHI. 
              thenBB = builder.GetInsertBlock()!;

              // after
              builder.SetInsertPoint(afterBB);
              return voidIndicator;
            },
          });
        },
        Struct: ({ structTy, fields }) => {
          const structPtr = builder.CreateAlloca(llvmTy(structTy).getPointerElementType());

          fields.forEach(({ name, value }) => {
            const fieldPtr = structFieldPtr(structTy, structPtr, name);
            builder.CreateStore(compileExpr(value), fieldPtr);
          });

          return structPtr;
        },
        StructAccess: ({ lhs, field, ty }) => {
          const structTy = MonoTy.deref(lhs.ty);
          assert(structTy.variant === 'Struct');
          const structPtr = compileExpr(lhs);
          const fieldPtr = structFieldPtr(structTy, structPtr, field);

          return builder.CreateLoad(llvmTy(ty), fieldPtr)
        },
        Array: ({ init, elemTy }) => {
          const { arrayStructPtr, arrayTy, arrayData } = allocateArray(elemTy, ArrayInit.len(init));

          match(init, {
            elems: ({ elems }) => {
              elems.forEach((elem, index) => {
                const elemPtr = arrayElementPtr(arrayTy, arrayData, index);
                builder.CreateStore(compileExpr(elem), elemPtr);
              });
            },
            fill: ({ value, count }) => {
              const elem = compileExpr(value);
              // counter
              const counter = builder.CreateAlloca(llvm.Type.getInt32Ty(context), builder.getInt32(1), 'counter');
              builder.CreateStore(llvm.ConstantInt.get(llvm.Type.getInt32Ty(context), 0), counter);
              const max = llvm.ConstantInt.get(llvm.Type.getInt32Ty(context), count);

              createWhileLoop(() => builder.CreateICmpULT(builder.CreateLoad(llvm.Type.getInt32Ty(context), counter), max), () => {
                const counterDeref = builder.CreateLoad(llvm.Type.getInt32Ty(context), counter);
                const elemPtr = arrayElementPtr(arrayTy, arrayData, counterDeref);
                builder.CreateStore(elem, elemPtr);

                // counter += 1                
                // counter += 1                
                // counter += 1                
                builder.CreateStore(
                  builder.CreateAdd(
                    counterDeref,
                    llvm.ConstantInt.get(llvm.Type.getInt32Ty(context), 1)
                  ),
                  counter
                );
              });
            },
          });

          return arrayStructPtr;
        },
      });
    }

    function compileStmt(stmt: Stmt): void {
      match(stmt, {
        Let: ({ name, value }) => {
          declareVar(name, compileExpr(value));
        },
        VariableAssignment: ({ name, value }) => {
          const v = resolveVar(name);
          assert(v.kind === 'mut');
          builder.CreateStore(compileExpr(value), v.ptr);
        },
        StructAssignment: ({ struct, structTy, field, value }) => {
          const structPtr = compileExpr(struct);
          const fieldPtr = structFieldPtr(structTy, structPtr, field);
          builder.CreateStore(compileExpr(value), fieldPtr);
        },
        Expr: ({ expr }) => { compileExpr(expr); },
        While: ({ condition, statements }) => {
          createWhileLoop(() => compileExpr(condition), () => {
            statements.forEach(stmt => {
              compileStmt(stmt);
            });
          });
        },
        Return: ({ expr }) => {
          assert(stacks.func.length > 0, 'empty func stack');
          const func = last(stacks.func);

          expr.do(expr => {
            const val = compileExpr(expr);
            if (func.returnVal != null) {
              builder.CreateStore(val, func.returnVal);
            }
          });

          if (stacks.controlFlow.length > 0) {
            last(stacks.controlFlow).exitBB = func.exitBB;
          } else {
            builder.CreateBr(func.exitBB);
          }
        },
      });
    }

    const structInstances: { row: Row, ty: LLVM.StructType }[] = [];

    function llvmTy(ty: MonoTy): llvm.Type {
      return match(MonoTy.expand(ty, module.typeContext), {
        Const: c => matchString<string, llvm.Type>(c.name, {
          int: () => {
            const kind = MonoTy.deref(c.args[0]);
            assert(kind.variant === 'Const', 'Underconstrained integer literal');

            return matchString(kind.name as IntKind, {
              i8: () => llvm.Type.getInt8Ty(context),
              u8: () => llvm.Type.getInt8Ty(context),
              i32: () => llvm.Type.getInt32Ty(context),
              u32: () => llvm.Type.getInt32Ty(context),
              i64: () => llvm.Type.getInt64Ty(context),
              u64: () => llvm.Type.getInt64Ty(context),
            });
          },
          void: () => llvm.Type.getVoidTy(context),
          bool: () => llvm.Type.getInt1Ty(context),
          ptr: () => llvm.PointerType.get(llvmTy(c.args[0]), 0),
          _: () => panic(`Unknown type representation for const type: ${c.name}`),
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

    function declareFunc(f: VariantOf<Decl, 'Function'>, isExternal?: boolean): LLVM.Function {
      const returnTy = llvmTy(f.returnTy);
      const argTys = f.args.map(a => llvmTy(a.name.ty));
      const funcTy = llvm.FunctionType.get(returnTy, argTys, false);
      const ext = isExternal ?? (
        f.pub ||
        f.name.original === 'main' ||
        f.attributes.has('extern')
      );
      const linkage = llvm.Function.LinkageTypes[ext ? 'ExternalLinkage' : 'PrivateLinkage'];
      const func = llvm.Function.Create(funcTy, linkage, f.name.mangled, mod);
      funcs.set(f.name.mangled, func);

      return func;
    }

    function compileDecl(decl: Decl): void {
      match(decl, {
        Function: f => {
          const proto = declareFunc(f);

          if (f.attributes.has('extern')) {
            return;
          }

          const returnsVoid = MonoTy.eq(f.returnTy, MonoTy.void());
          const entry = llvm.BasicBlock.Create(context, 'entry', proto);
          const parent = entry.getParent()!;
          builder.SetInsertPoint(entry);

          scoped(() => {
            f.args.forEach((arg, index) => {
              const llvmArg = proto.getArg(index);
              llvmArg.setName(arg.name.mangled);
              declareVar(arg.name, llvmArg);
            });

            return f.body.match({
              Some: body => {
                const retTy = llvmTy(f.returnTy);
                // if f contains at least one early return statement
                // then allocate the space for the return value 
                // then allocate the space for the return value 
                // then allocate the space for the return value 
                const returnVal = block(() => {
                  if (f.canReturnEarly && !returnsVoid) {
                    return builder.CreateAlloca(retTy, null, 'return_val');
                  }
                });

                const exitBB = llvm.BasicBlock.Create(context, 'exit');

                stacks.func.push({ returnVal, exitBB });
                const ret = compileExpr(body);
                if (f.canReturnEarly && returnVal != null) {
                  builder.CreateStore(ret, returnVal);
                }
                stacks.func.pop();

                if (f.canReturnEarly) {
                  builder.CreateBr(exitBB);
                  parent.insertAfter(entry, exitBB);
                  builder.SetInsertPoint(exitBB);

                  if (returnsVoid) {
                    builder.CreateRetVoid();
                  } else {
                    assert(returnVal != null, 'returnVal == null');
                    builder.CreateRet(builder.CreateLoad(retTy, returnVal));
                  }
                } else {
                  if (returnsVoid) {
                    builder.CreateRetVoid();
                  } else {
                    builder.CreateRet(ret);
                  }
                }

                return ret;
              },
              None: () => {
                if (f.attributes.has('meta')) {
                  const ret = meta(
                    f.attributes.get('meta')!.args[0],
                    proto,
                    builder,
                    llvm,
                    context,
                  );

                  assert(ret.isNone() ? returnsVoid : !returnsVoid);
                  ret.match({
                    Some: ret => {
                      builder.CreateRet(ret);
                    },
                    None: () => {
                      builder.CreateRetVoid();
                    },
                  });
                } else {
                  return panic(`Function prototype for '${f.name.original}' is missing a meta or extern attribute`);
                }
              },
            });
          });

          if (llvm.verifyFunction(proto)) {
            panic('Function verification failed');
          }
        },
        TypeAlias: () => { },
        Import: () => { },
        _: () => { },
      });
    }

    declareImports();
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

  return { compile };
};
