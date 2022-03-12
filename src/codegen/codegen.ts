import { DataType, match, VariantOf } from "itsamatch";
import { Decl, Expr, Prog, Stmt } from "../ast/bitter";
import { Name } from "../ast/name";
import { MonoTy } from "../infer/types";
import { last, reverse } from "../utils/array";
import { Maybe } from "../utils/maybe";
import { panic } from "../utils/misc";
import { Func } from "./wasm/func";
import { Inst } from "./wasm/instructions";
import { Expr as IRExpr } from './wasm/ir';
import { Module } from "./wasm/sections";
import { BlockType, FuncIdx, FuncType, GlobalIdx, LocalIdx } from "./wasm/types";
import { blockRetTy, wasmTy } from "./wasm/utils";

export class Compiler {
  private mod: Module;
  private funcStack: Func[];
  private globalVars: GlobalVar[];
  private topLevelFuncs: Map<string, Func>;

  private constructor() {
    this.mod = Module.make();
    this.funcStack = [];
    this.globalVars = [];
    this.topLevelFuncs = new Map();
  }

  private compileDecl(decl: Decl): void {
    match(decl, {
      Function: f => {
        this.compileFunction(f);
      },
      Module: mod => {
        // declare top-level functions first
        mod.decls.forEach(decl => {
          match(decl, {
            Function: f => {
              this.declareFunction(f);
            },
            Impl: impl => {
              impl.decls.forEach(decl => {
                if (decl.variant === 'Function') {
                  this.declareFunction(decl);
                }
              });
            },
            TraitImpl: impl => {
              impl.methods.forEach(method => {
                if (method.variant === 'Function') {
                  this.declareFunction(method);
                }
              });
            },
            _: () => { },
          });
        });

        mod.decls.forEach(decl => {
          this.compileDecl(decl);
        });
      },
      Impl: ({ decls }) => {
        decls.forEach(decl => {
          this.compileDecl(decl);
        });
      },
      TraitImpl: ({ methods }) => {
        methods.forEach(method => {
          this.compileDecl(method);
        });
      },
      Trait: () => { },
      _: () => {
        panic('unhandled decl variant: ' + decl.variant);
      },
      Use: () => { },
    });
  }

  private declareFunction(f: VariantOf<Decl, 'Function'>): FuncIdx {
    const idx: FuncIdx = this.topLevelFuncs.size;
    const func = Func.make(
      f.name.renaming,
      f.args.map(({ name, mutable }) => ({ name: name.original, ty: name.ty, mutable })),
      f.body,
      idx
    );

    if (this.topLevelFuncs.has(f.name.renaming)) {
      panic(`function ${f.name.renaming} already defined`);
    }

    this.topLevelFuncs.set(f.name.renaming, func);

    return idx;
  }

  private compileFunction(f: VariantOf<Decl, 'Function'>): FuncIdx {
    if (!this.topLevelFuncs.has(f.name.renaming)) {
      panic(`function ${f.name.renaming} not defined`);
    }

    const func = this.topLevelFuncs.get(f.name.renaming)!;

    this.funcStack.push(func);
    const insts = IRExpr.compile(this.compileExpr(f.body));
    this.funcStack.pop();

    const funcIdx = Module.addFunction(
      this.mod,
      f.name.renaming,
      FuncType.make(f.args.flatMap(arg => wasmTy(arg.name.ty)), [wasmTy(f.body.ty)]),
      Func.locals(func),
      insts,
      true
    );

    if (funcIdx !== func.index) {
      panic(`mismatching function index for '${f.name.renaming}': ${funcIdx} !== ${func.index}`);
    }

    return func.index;
  }

  private call(funcName: Name, args: Expr[]): IRExpr {
    return match(this.resolveVar(funcName), {
      Func: ({ idx }) => IRExpr.call(idx, args.map(a => this.compileExpr(a))),
      _: () => panic('indirect function call not supported yet'),
    });
  }

  private compileExpr(expr: Expr): IRExpr {
    return match(expr, {
      Const: c => match(c.value, {
        u32: ({ value }) => IRExpr.i32(value),
        bool: ({ value }) => IRExpr.bool(value),
        unit: () => IRExpr.unit(),
      }),
      Block: ({ statements, lastExpr, ty }) => {
        const retTy = blockRetTy(ty);
        const exprs = statements.map(s => this.compileStmt(s));
        lastExpr.do(e => {
          exprs.push(this.compileExpr(e));
        });

        return IRExpr.block(retTy, exprs);
      },
      BinaryOp: ({ lhs, op, rhs }) => {
        return IRExpr.binop(this.compileExpr(lhs), op, this.compileExpr(rhs));
      },
      UnaryOp: ({ op, expr }) => {
        return IRExpr.unop(op, this.compileExpr(expr));
      },
      Variable: ({ name }) => {
        return match(this.resolveVar(name), {
          Local: ({ idx }) => IRExpr.resolveVar(idx),
          Global: ({ idx }) => IRExpr.resolveVar(idx, { isGlobal: true }),
          Func: ({ idx }) => IRExpr.i32(idx),
        });
      },
      IfThenElse: ({ condition, then, else_, ty }) => {
        return IRExpr.if(
          blockRetTy(ty),
          this.compileExpr(condition),
          this.compileExpr(then),
          else_.map(e => this.compileExpr(e)),
        );
      },
      Assignment: ({ lhs, rhs }) => {
        return match(lhs, {
          Variable: ({ name }) => {
            return match(this.resolveVar(name), {
              Local: ({ idx }) => IRExpr.assignment(idx, this.compileExpr(rhs)),
              Global: ({ idx }) => IRExpr.assignment(idx, this.compileExpr(rhs), { isGlobal: true }),
              Func: () => panic('cannot assign to a function'),
            });
          },
          _: () => panic('unhandled assignment target: ' + Expr.showSweet(lhs)),
        });
      },
      Call: ({ lhs, args }) => {
        return match(lhs, {
          Variable: ({ name }) => this.call(name, args),
          _: () => panic('unhandled call target: ' + Expr.showSweet(lhs)),
        });
      },
      MethodCall: ({ receiver, method, args, impl }) => {
        const name = impl.unwrap().methods[method].name;
        return this.call(name, [receiver, ...args]);
      },
      WasmBlock: ({ instructions }) => {
        const insts: Inst[] = instructions.flatMap(inst => inst.match({
          left: inst => [inst],
          right: ([expr]) => IRExpr.compile(this.compileExpr(expr)),
        }));

        return IRExpr.raw(...insts);
      },
      While: ({ condition, body }) => {
        const block = (body as VariantOf<Expr, 'Block'>);
        const stmts = block.statements;
        block.lastExpr.do(expr => {
          stmts.push(Stmt.Expr(expr));
        });

        return IRExpr.while(
          this.compileExpr(condition),
          stmts.flatMap(s => this.compileStmt(s)),
        );
      },
      _: () => {
        return panic(`expr ${expr.variant} not supported yet`);
      }
    });
  }

  private get currentFunc(): Func {
    if (this.funcStack.length === 0) {
      panic('no current function');
    }

    return last(this.funcStack);
  }

  private declareVar(name: string, ty: MonoTy, mutable: boolean): DeclaredVar {
    if (this.funcStack.length === 0) {
      const globalIdx = this.globalVars.length;
      this.globalVars.push({ name, ty, mutable, index: globalIdx });
      return DeclaredVar.Global(globalIdx);
    } else {
      return DeclaredVar.Local(Func.declareVar(this.currentFunc, name, ty, mutable));
    }
  }

  private resolveVar(name: Name): ResolvedVar {
    return Maybe.firstSomeBy(reverse(this.funcStack), f => Func.resolveVar(f, name.renaming)).match({
      Some: ([funcIdx, index]) => {
        if (index !== 0) {
          panic('closures not supported yet');
        }

        return ResolvedVar.Local(funcIdx);
      },
      None: () => {
        const globalVar = this.globalVars.find(v => v.name === name.renaming);
        if (globalVar) {
          return ResolvedVar.Global(globalVar.index);
        }

        if (this.topLevelFuncs.has(name.renaming)) {
          return ResolvedVar.Func(this.topLevelFuncs.get(name.renaming)!.index);
        }

        return panic(`resolveVar: variable ${name.renaming} not found`);
      },
    });
  }

  private compileStmt(stmt: Stmt): IRExpr {
    return match(stmt, {
      Expr: ({ expr }) => {
        if (wasmTy(expr.ty) !== 'none') {
          return IRExpr.dropValue(this.compileExpr(expr));
        } else {
          return this.compileExpr(expr);
        }
      },
      Let: ({ name, expr, mutable }) => {
        return match(this.declareVar(name.original, expr.ty, mutable), {
          Local: ({ idx }) => IRExpr.declareVar(idx, this.compileExpr(expr)),
          Global: ({ idx }) => IRExpr.declareVar(idx, this.compileExpr(expr), { isGlobal: true }),
        });
      },
      _: () => {
        panic(`stmt ${stmt.variant} not supported yet`);
        return IRExpr.unreachable();
      },
    });
  }

  public static compile(prog: Prog): Module {
    const compiler = new Compiler();

    prog.forEach(decl => {
      compiler.compileDecl(decl);
    });

    return compiler.mod;
  }
}

type GlobalVar = {
  name: string,
  ty: MonoTy,
  mutable: boolean,
  index: GlobalIdx
};

type DeclaredVar = DataType<{
  Local: { idx: LocalIdx },
  Global: { idx: number },
}>;

const DeclaredVar = {
  Local: (idx: LocalIdx): DeclaredVar => ({ variant: 'Local', idx }),
  Global: (idx: number): DeclaredVar => ({ variant: 'Global', idx }),
};

type ResolvedVar = DataType<{
  Local: { idx: LocalIdx },
  Global: { idx: number },
  Func: { idx: FuncIdx },
}>;

const ResolvedVar = {
  ...DeclaredVar,
  Func: (idx: FuncIdx): ResolvedVar => ({ variant: 'Func', idx }),
};