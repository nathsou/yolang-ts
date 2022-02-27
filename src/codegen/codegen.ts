import { match, VariantOf } from "itsamatch";
import { Decl, Expr, Prog, Stmt } from "../ast/bitter";
import { MonoTy } from "../infer/types";
import { findRev, last } from "../utils/array";
import { Maybe } from "../utils/maybe";
import { matchString, panic } from "../utils/misc";
import { Expr as IRExpr } from './wasm/ir';
import { Locals, Module } from "./wasm/sections";
import { BlockType, FuncType, LocalIdx, ValueType } from "./wasm/types";

const wasmTy = (ty: MonoTy): ValueType => match(ty, {
  Const: c => matchString(c.name, {
    'u32': () => ValueType.i32(),
    'bool': () => ValueType.i32(),
    '()': () => ValueType.none(),
    _: () => {
      return panic(`Unknown type repr for const type: ${c.name}`);
    },
  }),
  Var: v => wasmTy(MonoTy.deref(v)),
  _: () => {
    return panic(`type repr not defined yet for ${MonoTy.show(ty)}`);
  },
});

const blockRetTy = (ty: MonoTy): BlockType => match(ty, {
  Const: c => matchString(c.name, {
    'u32': () => BlockType.ValueType('i32'),
    'bool': () => BlockType.ValueType('i32'),
    '()': () => BlockType.ValueType('none'),
    _: () => {
      panic(`Unknown type repr for const type: ${c.name}`);
      return BlockType.Void();
    },
  }),
  Var: v => blockRetTy(MonoTy.deref(v)),
  _: () => {
    panic(`type repr not defined yet for ${MonoTy.show(ty)}`);
    return BlockType.Void();
  },
});

type Var = {
  name: string,
  ty: MonoTy,
  mutable: boolean,
  scopeDepth: number,
  index: number,
};

type Func = {
  name: string,
  locals: Var[],
  body: Expr,
  scopeDepth: number,
};

const Func = {
  make: (name: string, args: { name: string, ty: MonoTy, mutable: boolean }[], body: Expr): Func => ({
    name,
    locals: args.map((a, index) => ({ ...a, index, scopeDepth: 0 })),
    body,
    scopeDepth: 0,
  }),
  declareVar: (self: Func, name: string, ty: MonoTy, mutable: boolean): LocalIdx => {
    self.locals.push({ name, ty, mutable, scopeDepth: self.scopeDepth, index: self.locals.length });
    return self.locals.length - 1;
  },
  scoped: (self: Func, f: () => void): void => {
    self.scopeDepth += 1;
    f();
    self.scopeDepth -= 1;
    self.locals = self.locals.filter(({ scopeDepth }) => scopeDepth <= self.scopeDepth);
  },
  resolveVar: (self: Func, name: string): Maybe<LocalIdx> => {
    return findRev(self.locals, ({ name: n }) => n === name).map(({ index }) => index);
  },
  locals: (self: Func): Locals => {
    return Locals.from(self.locals.flatMap(({ name, ty }) => ({ name, ty: wasmTy(ty) })));
  },
};

export class Compiler {
  private mod: Module;
  private funcStack: Func[];

  private constructor() {
    this.mod = Module.make();
    this.funcStack = [];
  }

  private compileDecl(decl: Decl): void {
    match(decl, {
      Function: f => {
        this.compileFunction(f);
      },
      Module: mod => {
        mod.decls.forEach(decl => {
          this.compileDecl(decl);
        });
      },
      _: () => {
        panic('unhandled decl variant: ' + decl.variant);
      },
    });
  }

  private compileFunction(f: VariantOf<Decl, 'Function'>): void {
    const func = Func.make(
      f.name.original,
      f.args.map(({ name, mutable }) => ({ name: name.original, ty: name.ty, mutable })),
      f.body
    );

    this.funcStack.push(func);
    const insts = IRExpr.compile(this.compileExpr(f.body));
    this.funcStack.pop();

    Module.addFunction(
      this.mod,
      f.name.renaming,
      FuncType.make(f.args.flatMap(arg => wasmTy(arg.name.ty)), [wasmTy(f.body.ty)]),
      Func.locals(func),
      insts,
      true
    );
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
        const localIdx = this.resolveVar(name.original);
        return IRExpr.resolveVar(localIdx);
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
            const localIdx = this.resolveVar(name.original);
            return IRExpr.assignment(localIdx, this.compileExpr(rhs));
          },
          _: () => panic('invalid assignment target: ' + Expr.showSweet(lhs)),
        });
      },
      _: () => {
        panic(`expr ${expr.variant} not supported yet`);
        return IRExpr.unreachable();
      }
    });
  }

  private get currentFunc(): Func {
    if (this.funcStack.length === 0) {
      panic('no current function');
    }

    return last(this.funcStack);
  }

  private declareVar(name: string, ty: MonoTy, mutable: boolean): LocalIdx {
    return Func.declareVar(this.currentFunc, name, ty, mutable);
  }

  private resolveVar(name: string): LocalIdx {
    return Func.resolveVar(this.currentFunc, name).match({
      Some: idx => idx,
      None: () => panic(`variable ${name} not found`),
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
        const localIdx = this.declareVar(name.original, expr.ty, mutable);
        return IRExpr.declareVar(localIdx, this.compileExpr(expr));
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
