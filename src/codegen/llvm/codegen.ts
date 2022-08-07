import { match, VariantOf } from 'itsamatch';
import type LLVM from 'llvm-bindings';
import { Decl, Expr, Stmt } from '../../ast/bitter';
import { VarName } from '../../ast/name';
import { Expr as SweetExpr } from '../../ast/sweet';
import { MonoTy } from '../../infer/types';
import { Const } from '../../parse/token';
import { last } from '../../utils/array';
import { matchString, panic } from '../../utils/misc';

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

  function compile(mod: VariantOf<Decl, 'Module'>): LLVM.Module {
    const context = new llvm.LLVMContext();
    const modules: LLVM.Module[] = [];
    const builder = new llvm.IRBuilder(context);
    const scopes: LexicalScope[] = [];
    const unit = Expr.Const(Const.unit(), SweetExpr.Const(Const.unit()));
    const unitTy = MonoTy.Const('()');
    const funcs = new Map<string, LLVM.Function>();

    function currentModule(): LLVM.Module {
      if (modules.length === 0) {
        panic('empty module stack');
      }

      return last(modules);
    }

    function compileModule(mod: VariantOf<Decl, 'Module'>): LLVM.Module {
      const module = new llvm.Module(mod.name, context);
      modules.push(module);

      mod.decls.forEach(d => { compileDecl(d); });
      if (llvm.verifyModule(module)) {
        panic('module verification failed');
      }

      modules.pop();

      return module;
    }

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
        const alloca = tmpBuilder.CreateAlloca(llvmpTy(name.ty), value, name.mangled);
        LexicalScope.declareMutableVar(currentScope(), name.mangled, alloca, name.ty);
        builder.CreateStore(value, alloca);
      } else {
        LexicalScope.declareImmutableVar(currentScope(), name.mangled, value, name.ty);
      }
    }

    function compileExpr(expr: Expr): llvm.Value {
      return match(expr, {
        Const: ({ value: c }) => match(c, {
          bool: ({ value }) => llvm.ConstantInt[value ? 'getTrue' : 'getFalse'](context),
          u32: ({ value }) => llvm.ConstantInt.get(llvm.Type.getInt32Ty(context), value),
          unit: () => llvm.Constant.getNullValue(llvm.Type.getInt1Ty(context)),
        }),
        UnaryOp: ({ op, expr }) => {
          switch (op) {
            case '-':
              return builder.CreateNeg(compileExpr(expr));
            case '!':
              return builder.CreateNot(compileExpr(expr));
          }
        },
        BinaryOp: ({ lhs, op, rhs }) => {
          const l = compileExpr(lhs);
          const r = compileExpr(rhs);

          switch (op) {
            case '+':
              return builder.CreateAdd(l, r);
            case '-':
              return builder.CreateSub(l, r);
            case '*':
              return builder.CreateMul(l, r);
            case '/':
              return builder.CreateSDiv(l, r);
            case '%':
              return builder.CreateSRem(l, r);
            case '==':
              return builder.CreateICmpEQ(l, r);
            case '!=':
              return builder.CreateICmpNE(l, r);
            case '<':
              return builder.CreateICmpULT(l, r);
            case '<=':
              return builder.CreateICmpULE(l, r);
            case '>':
              return builder.CreateICmpUGT(l, r);
            case '>=':
              return builder.CreateICmpUGE(l, r);
            case '&&':
              return builder.CreateAnd(l, r);
            case '||':
              return builder.CreateOr(l, r);
            case '|':
              return builder.CreateOr(l, r);
            case '&':
              return builder.CreateAnd(l, r);
          }
        },
        Variable: ({ name }) => {
          const v = resolveVar(name);

          if (v.kind === 'immut') {
            return v.value;
          } else {
            return builder.CreateLoad(llvmpTy(v.ty), v.ptr);
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
            const phiNode = builder.CreatePHI(llvmpTy(ty), 2, 'if');
            phiNode.addIncoming(thenValue, thenBB);
            phiNode.addIncoming(elseValue, elseBB);

            return phiNode;
          } else {
            return compileExpr(unit);
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

          return compileExpr(unit);
        },
        Assignment: ({ lhs, rhs }) => {
          return match(lhs, {
            Variable: ({ name }) => {
              const v = resolveVar(name);
              if (v.kind === 'mut') {
                return builder.CreateStore(compileExpr(rhs), v.ptr);
              } else {
                return panic(`expected mutable variable in assignment expression: ${Expr.showSweet(expr)}`);
              }
            },
            _: () => panic('unhandled assignment target: ' + Expr.showSweet(lhs)),
          });
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

    function llvmpTy(ty: MonoTy): llvm.Type {
      return match(ty, {
        Const: c => matchString<string, llvm.Type>(c.name, {
          'u32': () => llvm.Type.getInt32Ty(context),
          'bool': () => llvm.Type.getInt1Ty(context),
          '()': () => llvm.Type.getInt1Ty(context),
          _: () => {
            return panic(`Unknown type repr for const type: ${c.name}`);
          },
        }),
        Var: v => match(v.value, {
          Link: ({ to }) => llvmpTy(MonoTy.deref(to)),
          Unbound: () => panic('llvmTy: found uninstantiated type variable'),
        }, 'kind'),
        _: () => panic('llvmpTy: ' + ty.variant + ' not implemented'),
      })
    }

    function scoped<T>(action: (scope: LexicalScope) => T): T {
      const scope = LexicalScope.make();
      scopes.push(scope);
      const ret = action(scope);
      scopes.pop();

      return ret;
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

          const returnTy = llvmpTy(f.body.ty);
          const argTys = f.args.map(a => llvmpTy(a.name.ty));
          const funcTy = llvm.FunctionType.get(returnTy, argTys, false);
          const func = llvm.Function.Create(funcTy, llvm.Function.LinkageTypes.ExternalLinkage, f.name.mangled, currentModule());
          funcs.set(f.name.mangled, func);
          const entry = llvm.BasicBlock.Create(context, 'entry', func);
          builder.SetInsertPoint(entry);
          const ret = scoped(scope => {
            f.args.forEach((arg, index) => {
              const llvmArg = func.getArg(index);
              llvmArg.setName(arg.name.mangled);
              LexicalScope.declareImmutableVar(scope, arg.name.mangled, llvmArg, arg.name.ty);
            });

            return compileExpr(f.body);
          });

          builder.CreateRet(ret);

          if (llvm.verifyFunction(func)) {
            panic('function verification failed');
          }
        },
        Module: mod => {
          compileModule(mod);
        },
        _: () => { },
      });
    }

    return compileModule(mod);
  }

  async function compileIR(module: LLVM.Module, outfile: string, optLevel: 0 | 1 | 2 | 3): Promise<string> {
    const { exec } = await import('node:child_process');

    module.setTargetTriple('wasm32-wasi');
    llvm.WriteBitcodeToFile(module, `${outfile}.bc`);

    const buildCommand = [
      'clang',
      `--target=wasm32-wasi`,
      '-nostdlib',
      `-O${optLevel}`,
      '-Wl,--no-entry',
      '-Wl,--export-all',
      `-Wl,--lto-O${optLevel}`,
      '-Wl,--allow-undefined',
      `-o ${outfile}.wasm`,
      `${outfile}.bc`,
    ].join(' ');

    return new Promise((resolve, reject) => {
      exec(buildCommand, (error, stdout) => {
        if (error != null) {
          reject(error);
        } else {
          resolve(stdout);
        }
      });
    });
  }

  return { compile, compileIR };
};