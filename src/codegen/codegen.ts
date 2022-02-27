import { match, VariantOf } from "itsamatch";
import { Decl, Expr, Prog, Stmt } from "../ast/bitter";
import { MonoTy } from "../infer/types";
import { matchString, panic } from "../utils/misc";
import { Expr as IRExpr } from './wasm/ir';
import { Module } from "./wasm/sections";
import { BlockType, FuncType, ValueType } from "./wasm/types";

const wasmTy = (ty: MonoTy): ValueType[] => match(ty, {
  Const: c => matchString(c.name, {
    'u32': () => [ValueType.i32()],
    'bool': () => [ValueType.i32()],
    _: () => {
      panic(`Unknown type repr for const type: ${c.name}`);
      return [];
    },
  }),
  Var: v => wasmTy(MonoTy.deref(v)),
  _: () => {
    panic(`type repr not defined yet for ${MonoTy.show(ty)}`);
    return [];
  },
});

const blockRetTy = (ty: MonoTy): BlockType => match(ty, {
  Const: c => matchString(c.name, {
    'u32': () => BlockType.ValueType('i32'),
    'bool': () => BlockType.ValueType('i32'),
    '()': () => BlockType.Void(),
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

export class Compiler {
  private mod: Module;

  private constructor() {
    this.mod = Module.make();
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
    Module.addFunction(
      this.mod,
      f.name.renaming,
      FuncType.make(f.args.flatMap(arg => wasmTy(arg.name.ty)), wasmTy(f.body.ty)),
      [],
      IRExpr.compile(this.compileExpr(f.body)),
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
      IfThenElse: ({ condition, then, else_, ty }) => {
        return IRExpr.if(
          blockRetTy(ty),
          this.compileExpr(condition),
          this.compileExpr(then),
          else_.map(e => this.compileExpr(e)),
        );
      },
      _: () => {
        panic(`expr ${expr.variant} not supported yet`);
        return IRExpr.unreachable();
      }
    });
  }

  private compileStmt(stmt: Stmt): IRExpr {
    return match(stmt, {
      Expr: ({ expr }) => IRExpr.dropValue(this.compileExpr(expr)),
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
