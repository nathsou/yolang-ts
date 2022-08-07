import { DataType, match, VariantOf } from "itsamatch";
import { Decl, Expr, Prog, Stmt } from "../../ast/bitter";
import { FuncName, VarName } from "../../ast/name";
import { Row } from "../../infer/records";
import { MonoTy, PolyTy } from "../../infer/types";
import { last, reverse } from "../../utils/array";
import { Maybe } from "../../utils/maybe";
import { panic } from "../../utils/misc";
import { Func } from "./func";
import { Inst } from "./instructions";
import { Expr as IRExpr } from './ir';
import { Module } from "./sections";
import { FuncIdx, FuncType, GlobalIdx, LocalIdx } from "./types";
import { blockRetTy, wasmTy } from "./utils";

export class Compiler {
  private mod: Module;
  private funcStack: Func[];
  private globalVars: GlobalVar[];
  private topLevelFuncs: Map<string, Func>;
  private structs: Map<string, { fields: { name: string, ty: MonoTy }[] }>;

  private constructor() {
    this.mod = Module.make();
    this.funcStack = [];
    this.globalVars = [];
    this.topLevelFuncs = new Map();
    this.structs = new Map();
  }

  private compileDecl(decl: Decl): void {
    match(decl, {
      Function: f => {
        if (!PolyTy.isPolymorphic(f.funTy)) {
          this.compileFunction(f);
        } else {
          f.instances.forEach(inst => {
            this.compileFunction(inst);
          });
        }
      },
      Module: mod => {
        // declare top-level functions first
        mod.decls.forEach(decl => {
          match(decl, {
            Function: f => {
              if (!PolyTy.isPolymorphic(f.funTy)) {
                this.declareFunction(f);
              } else {
                f.instances.forEach(inst => {
                  this.declareFunction(inst);
                });
              }
            },
            _: () => { },
          });
        });

        mod.decls.forEach(decl => {
          this.compileDecl(decl);
        });
      },
      TypeAlias: ({ name, alias }) => {
        match(alias, {
          Record: ({ row }) => {
            if (!this.structs.has(name)) {
              this.structs.set(name, { fields: Row.fields(row).map(([name, ty]) => ({ name, ty })) });
            } else {
              panic(`struct ${name} already defined`);
            }
          },
          _: () => { },
        })
      },
      _: () => {
        panic('unhandled decl variant: ' + decl.variant);
      },
      Use: () => { },
    });
  }

  private declareFunction(f: VariantOf<Decl, 'Function'>): FuncIdx {
    const idx: FuncIdx = this.topLevelFuncs.size;
    const func = Func.make(
      f.name.mangled,
      f.args.map(({ name, mutable }) => ({ name: name.original, ty: name.ty, mutable })),
      f.body,
      idx
    );

    if (this.topLevelFuncs.has(f.name.mangled)) {
      panic(`function ${f.name.mangled} already defined`);
    }

    this.topLevelFuncs.set(f.name.mangled, func);

    return idx;
  }

  private compileFunction(f: VariantOf<Decl, 'Function'>): FuncIdx {
    if (!this.topLevelFuncs.has(f.name.mangled)) {
      panic(`function ${f.name.mangled} not defined`);
    }

    const func = this.topLevelFuncs.get(f.name.mangled)!;

    this.funcStack.push(func);
    const insts = IRExpr.compile(this.compileExpr(f.body));
    this.funcStack.pop();

    const funcIdx = Module.addFunction(
      this.mod,
      f.name.mangled,
      FuncType.make(f.args.flatMap(arg => wasmTy(arg.name.ty)), [wasmTy(f.body.ty)]),
      Func.locals(func),
      insts,
      true
    );

    if (funcIdx !== func.index) {
      panic(`mismatching function index for '${f.name.mangled}': ${funcIdx} !== ${func.index}`);
    }

    return func.index;
  }

  private call(funcName: FuncName, args: Expr[]): IRExpr {
    if (this.topLevelFuncs.has(funcName.mangled)) {
      const f = this.topLevelFuncs.get(funcName.mangled)!;
      return IRExpr.call(f.index, args.map(a => this.compileExpr(a)));
    } else {
      return panic('indirect function call not supported yet: ' + funcName.mangled)
    }
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
      NamedFuncCall: ({ name, args }) => {
        return this.call(name.unwrapRight('codegen: unresolved function name'), args);
      },
      Call: ({ lhs }) => {
        return panic('unhandled call target: ' + Expr.showSweet(lhs));
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

  private resolveVar(name: VarName): ResolvedVar {
    return Maybe.firstSomeBy(reverse(this.funcStack), f => Func.resolveVar(f, name.mangled)).match({
      Some: ([funcIdx, index]) => {
        if (index !== 0) {
          panic('closures not supported yet');
        }

        return ResolvedVar.Local(funcIdx);
      },
      None: () => {
        const globalVar = this.globalVars.find(v => v.name === name.mangled);
        if (globalVar) {
          return ResolvedVar.Global(globalVar.index);
        }

        if (this.topLevelFuncs.has(name.mangled)) {
          return ResolvedVar.Func(this.topLevelFuncs.get(name.mangled)!.index);
        }

        return panic(`resolveVar: variable ${name.mangled} not found`);
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
      Let: ({ name, expr, mutable }) => match(this.declareVar(name.original, expr.ty, mutable), {
        Local: ({ idx }) => IRExpr.declareVar(idx, this.compileExpr(expr)),
        Global: ({ idx }) => IRExpr.declareVar(idx, this.compileExpr(expr), { isGlobal: true }),
      }),
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